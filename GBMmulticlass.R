# TAIWAN POLICE - using GBM to multiclass triads
# 
# change this to your working directory

setwd("/Users/jse/DEsktop/TAIWAN POLICE")

library(gbm)
library(ggplot2)

## LOAD id file
id = read.csv("id.csv")
dim(id)
id[1:4,]
summary(id)

## LOAD case file
crp = read.csv("CRP.csv",encoding="utf-8")
dim(crp)
crp[1:4,]
as.character(crp[4,])
summary(crp)  # took it as levels ok
df[1,]
colnames(crp)[3] <- c("ID")


### LEFT JOIN  id.csv + crp.csv
df <- merge(x = crp[!duplicated(crp$ID),], y = id, by = "ID", all.y = TRUE)
dim(df)
dim(id)
dim(crp)  # OK
df [1:22,1:9] 
length(unique(id$ID))


## VIZ
hist(id$target)
qplot(id[!id$target=="Non",]$target, stat="count") 
ggplot(id,target)
df[1,]
g <- ggplot(df, aes(Plane)) + geom_bar()+ coord_flip()
g <- ggplot(df, aes(CRP)) + geom_bar()+ coord_flip()
g <- ggplot(df, aes(TypeIIIDrugs)) + geom_bar()+ coord_flip()
g <- ggplot(df, aes(target)) + geom_bar()+ coord_flip()
g <- ggplot(df, aes(Role,fill=Role)) + geom_bar()+ coord_flip()
g


# PREPARE
df <- df[,-c(1)] # remove index
df <- df[,-c(16)] # remove address too many levels GBM cannot handle
df <- df[!(df$target=="Non"),]
df$Group <- c()  # data leak
df$caseno <- c()  # data leak
dim(df)
df[1:3,]


#SPLIT
df <- df[sample(nrow(df)),]  # shuffle
df.train <- df[1:150,]
df.test <- df[151:329,]


## TRAIN
BST = gbm(target~.,data=df.train,
         distribution='multinomial',
         n.trees=200,
         interaction.depth=4,
         #cv.folds=5,
         shrinkage=0.005)

summary(BST)

df.train$target

predBST = predict(BST,n.trees=200, newdata=df.test,type='response')

predBST[1:6,,]

df.test[1:6,]


## COMPUTE PRECISION
solution <- as.factor(colnames(predBST)[apply(predBST,1,which.max)])
df.test$target
solution.num <- rep(0,length(solution))
solution.num[as.character(solution) == as.character(df.test$target)] <- 1
mean(solution.num)
head(predBST)

# id.csv + crp.csv (first case only)  46.9%
# id.csv            40%
