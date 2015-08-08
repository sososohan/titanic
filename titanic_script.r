train_script = read.csv("titanic/train.csv")
test_script = read.csv("titanic/test.csv")
test_script2 <- test_script		#need to change survived to zero to get rid of NA's
test_script2$Survived = 0
all_script <-rbind(train_script, test_script2)
all_script$Fare[is.na(all_script$Fare)] <- median(all_script$Fare, na.rm=TRUE)
library(rpart)
AgeFit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,data=all_script[!is.na(all_script$Age),], method="anova")
all_script$Age[is.na(all_script$Age)] <- predict(AgeFit, all_script[is.na(all_script$Age),])
train_script <-all_script[1:891,]
test_script2 <- all_script[892:1309,]
test_script$Age <- test_script2$Age
test_script$Fare <- test_script2$Fare
train_script$FamSize <- train_script$SibSp + train_script$Parch + 1
test_script$FamSize <- test_script$SibSp + test_script$Parch + 1
train_script$cabinLetter = as.factor(substring(train_script$Cabin,1,1))
test_script$cabinLetter = as.factor(substring(test_script$Cabin,1,1))
levels(test_script$Name) <- levels(train_script$Name)
levels(test_script$Ticket) <- levels(train_script$Ticket)
levels(test_script$Cabin) <- levels(train_script$Cabin)
levels(test_script$Embarked) <- levels(train_script$Embarked)
levels(test_script$FamSize) <- levels(train_script$FamSize)
levels(test_script$cabinLetter) <- levels(train_script$cabinLetter)
library(randomForest)
set.seed(50)
myForest_script <- randomForest(as.factor(Survived)~ Pclass + Sex + Age + SibSp + Parch + Fare + cabinLetter + Embarked + FamSize, data=train_script, importance=TRUE, ntree=2000)
prediction_script <- predict(myForest_script, test_script)
submit_script <- data.frame(PassengerId = test_script$PassengerId, Survived = prediction_script)
table(submit_script$Survived)
write.csv(submit_script, file = "titanic/forest_script_one.csv", row.names = FALSE)