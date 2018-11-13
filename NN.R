# R Neural Network for auctioning robot
## First Test

#install.packages('MASS')
#install.packages("caTools")
#install.packages("neuralnet"
#install.packages("ggplot2")

library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)

#check:
#head(Boston)

#Always normallize data for neural net
##Scale using min-max

MyMaxs = apply(Boston,2,max)
MyMins = apply(Boston,2,min)

###FROM HELP: scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
###scale(x, center = TRUE, scale = TRUE)

ScaledMatrix = scale(Boston,center=MyMins, scale = MyMaxs-MyMins)
ScaledMatrix = as.data.frame(ScaledMatrix)

#head(ScaledMatrix)

###"Center" argument is the value subracted from each single data point in the column 
###"Scale" argument = each value in DF will be divided by the scale

#Split Data into Train vs Test 

#We are trying to predict medianvalue ... split the data with 70% of it being held for training
split = sample.split(ScaledMatrix$medv, SplitRatio = 0.7)
train = subset(ScaledMatrix, split==T)
test = subset(ScaledMatrix,split==F)

#Now we train the model
## NOTE: neuralnet package wont read formula

n = names(train)
n

#automatically joins all column names into a formula 
## Grabbing every single column name, pasting it all together as a formula 
f = as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f

#Result: medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat

#Training the NN
##NOTE:
##For classification, linear output is FALSE
##Research hidden layers for appropriate numbers ... we used first 5 then 3 (See plot)

NN = neuralnet(f,data = train, hidden = c(6,4,2), linear.output = FALSE)
plot(NN)

#Predictions

MyPredictions = neuralnet::compute(NN,test[1:13])

#Look at structure of predictions
str(MyPredictions)


##True predictions

### Here we are inverting our scaling (normalization) ... instead of dividing and subtracting
### We are multiplying and adding for non scaled predicitions

TruePredictions = MyPredictions$net.result * (max(Boston$medv)-min(Boston$medv)+min(Boston$medv))

### Conver the test data to get a Mean Squared Error
Test.R = (test$medv)*(max(Boston$medv)-min(Boston$medv)+min(Boston$medv))

MSE.NN = sum((Test.R - TruePredictions)^2)/nrow(test)
MSE.NN

#Create DF for plot

ErrorDF = data.frame(Test.R, TruePredictions)
head(ErrorDF)

#plot
ggplot(ErrorDF,aes(x=Test.R,y=TruePredictions)) + geom_point() + stat_smooth()


#****NOTES: NN look a lot like BlackBoxes 


