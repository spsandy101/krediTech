
rm(list = ls())
# Get data
# training set
train<- read.csv("Training.csv", header = TRUE, sep = ";")
train$classlabel<- gsub(".", "", train$classlabel, fixed = TRUE)
train<- sapply(train, function(x){gsub(",", ".",x, fixed = TRUE)})
train<- as.data.frame(train)

train$v12<- as.numeric(as.character(train$v12))
train$v50<- as.numeric(as.character(train$v50))
train$v55<- as.integer(as.character(train$v55))
train$v20<- as.numeric(as.character(train$v20))
train$v24<- as.integer(as.character(train$v24))
train$v97<- as.numeric(as.character(train$v97))
train$v42<- as.integer(as.character(train$v42))
train$v53<- as.integer(as.character(train$v53))
train$v9<- as.integer(as.character(train$v9))


# testing set
test<- read.csv("Validation.csv", header = TRUE, sep = ";")
test$classlabel<- gsub(".", "", test$classlabel, fixed = TRUE)
test<- sapply(test, function(x){gsub(",", ".",x, fixed = TRUE)})
test<- as.data.frame(test)

test$v12<- as.numeric(as.character(test$v12))
test$v50<- as.numeric(as.character(test$v50))
test$v55<- as.integer(as.character(test$v55))
test$v20<- as.numeric(as.character(test$v20))
test$v24<- as.integer(as.character(test$v24))
test$v97<- as.numeric(as.character(test$v97))
test$v42<- as.integer(as.character(test$v42))
test$v53<- as.integer(as.character(test$v53))
test$v9<- as.integer(as.character(test$v9))

# Bring the data to a coherent form
data.combined<- rbind(train, test)

#Check for missing value
matrix_na<- as.array(sapply(data.combined, function(x){
  table(is.na(x))
}))

# Impute the "NA" values
library(missForest)
data.imputed<- missForest(data.combined, maxiter = 10, ntree = 100, variablewise = TRUE,
                          decreasing = FALSE, verbose = TRUE,
                          mtry = floor(sqrt(ncol(data.combined))), replace = TRUE)
class(data.imputed)
class(data.imputed$ximp)

# Make "train" and "test" datasets from imputed data
train.imputed<- data.imputed$ximp[c(1:3700), ]
test.imputed<- data.imputed$ximp[c(3701:3900),]
row.names(test.imputed)<- c(1:200)


#####################################################
# Train a random forest model
library(randomForest)

# Run a randoForest model
rf_train<- randomForest(classlabel~., data=train.imputed, ntree=50, importance=TRUE, replace=TRUE)

# Check for variable importance
rf_train$confusion
rf_train$importance
varImpPlot(rf_train, sort = TRUE)
varImpPlot(rf_train, sort = TRUE, n.var = 10)

### Make Predictions ###
test.imputed$prediction<- predict(rf_train, test.imputed[,c(-22)])
summary(test.imputed$prediction)
table(test.imputed$classlabel, test.imputed$prediction )
rf_train$confusion
