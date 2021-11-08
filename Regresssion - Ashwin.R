#LINEAR & POLYNOMIAL REGRESSION

dataset=read.csv('50_Startups.csv')
print(head(dataset,5))
library(caTools)
library(ggplot2)
library(ggpubr)

set.seed(123)
dataset$State=factor(x=dataset$State,
                     levels=c("New York", "California" ,"Florida"),labels=c('1','2','3'))
split=sample.split(dataset$Profit,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Fitting model into training set - All in

regressor=lm(formula=Profit~R.D.Spend+ Administration + Marketing.Spend + State ,data=training_set)
summary(regressor)

regressor=lm(formula=Profit~R.D.Spend + Marketing.Spend,data=training_set)
summary(regressor)

y_pred=predict(regressor,newdata=test_set)
final=cbind(test_set,y_pred)

ggplot()+geom_line(aes(x=final$Profit,y=final$y_pred),colour='black') + 
                   ggtitle('Profit predictions using Linear Regression') + xlab('Profit') + ylab('Profit') + theme_minimal()

#Polynomial Regression
dataset=read.csv("Position_Salaries.csv")
library(caTools)
library(ggplot2)
library(ggpubr)

ggplot()+ geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='black') + 
  ggtitle('Levels vs Salary')+xlab('Levels')+ylab('Salary') +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

linear_regressor=lm(formula=Salary~Level,data=dataset)
summary(linear_regressor)

dataset$Level2=dataset$Level*dataset$Level
dataset$Level3=dataset$Level2*dataset$Level
dataset$Level4=dataset$Level2*dataset$Level2

poly_regressor=lm(formula=Salary~Level+Level2+Level3+Level4,data=dataset)
lin_pred=predict(linear_regressor,newdata=dataset)
poly_pred=predict(poly_regressor,newdata=dataset)
Fin=cbind(dataset,lin_pred,poly_pred)

summary(poly_regressor)


colors <- c("Original" = "black", "LinReg" = "red", "PolyReg" = "blue")

ggplot()+ geom_point(aes(x=Fin$Level,y=Fin$Salary, colour='Original'))+
  geom_line(aes(x=Fin$Level,y=Fin$lin_pred, colour='LinReg')) + 
  geom_line(aes(x=Fin$Level,y=Fin$poly_pred, colour='PolyReg')) + 
  ggtitle('Levels vs Salary') + labs(x = "Levels",y = "Salary",colour = "Legend") +
  scale_colour_manual(name = 'Models', values = colors) + 
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

lin_pred_chk=predict(linear_regressor,data.frame(Level=6.5))
poly_pred_chk=predict(poly_regressor,data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3,Level4=6.5^4))

print(lin_pred_chk)
print(poly_pred_chk)

#It is quite close to the claim of 160k, so he is not bluffing

#SVR REGRESSION
dataset=read.csv("Position_Salaries.csv")
head(dataset)
library(caTools)
library(ggplot2)
library(ggpubr)
#install.packages('e1071')
library(e1071)

dataset=dataset[,2:3]
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
dataset$Level4=dataset$Level^4

regressor=svm(formula=Salary~.,data=dataset,type='eps-regression')
lin_reg=lm(formula=Salary~Level,data=dataset)
poly_reg=lm(formula=Salary~.,data=dataset)

ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='black') +
  geom_line(aes(x=dataset$Level,y=predict(regressor,newdata=dataset)),colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata=dataset)),colour='blue') +
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata=dataset)),colour='gray') +
  xlab('Level') + ylab('Salary') +ggtitle('Salary predictions by different models')

#DECISION TREE REGRESSION
dataset=read.csv("Position_Salaries.csv")
library(caTools)
library(ggplot2)
library(ggpubr)
#install.packages('rpart')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

dtree=rpart(formula = Salary~Level,data=dataset,minsplit=1)
summary(dtree)
fancyRpartPlot(dtree, caption = NULL)

regressor=rpart(formula=Salary~Level,data=dataset,control=rpart.control(minsplit = 1))
summary(regressor)

# Increase resolution in the plot by creating dummy dependent variables between 1 to 10 Level 
x_grid=seq(min(dataset$Level), max(dataset$Level), 0.0001)

ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='black') +
  geom_line(aes(x=x_grid,y=predict(dtree,newdata=data.frame(Level=x_grid))),colour='red')+
  xlab('Level') + ylab('Salary') +ggtitle('Salary predictions by Decision Tree') +
  theme_() + theme(plot.title = element_text(hjust = 0.5))

install.packages("ggthemes")
library(ggthemes)
#Increase resolution in the plot by creating dummy dependent variables between 1 to 10 Level 
x_grid=seq(min(dataset$Level),max(dataset$Level),0.0001)

ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='black', size = 1.5) +
  geom_line(aes(x=x_grid,y=predict(regressor,newdata=data.frame(Level=x_grid))),colour='red', size = 1.25)+
  xlab('Level') + ylab('Salary') +ggtitle('Salary predictions by Decision Tree') +
  theme_economist() + theme(plot.title = element_text(hjust = 0.5)) 

predict(regressor,newdata=data.frame(Level=6.5))

#RANDOM FOREST REGRESSION
#Split data into K, build multiple Dtrees, predict the value of test data based on these trees and take the average of all the outputs

dataset=read.csv("Position_Salaries.csv")
library(caTools)
library(ggplot2)
library(ggpubr)
library(rpart)
#install.packages('randomForest')
library(randomForest)
dtree=rpart(formula=Salary~Level,data=dataset,control=rpart.control(minsplit = 1))
regressor=randomForest(x=dataset[2],y=dataset$Salary,ntree=700)
set.seed(1234)

x_grid=seq(min(dataset$Level),max(dataset$Level),0.001)

ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='black') +
  geom_line(aes(x=x_grid,y=predict(regressor,newdata=data.frame(Level=x_grid))),colour='red')+
  geom_line(aes(x=x_grid,y=predict(dtree,newdata=data.frame(Level=x_grid))),colour='blue')+
  xlab('Level') + ylab('Salary') +ggtitle('Salary predictions by Random Forest/Decision Tree')

#Lot more steps/intervals in RFR as compared to a normal Dtree