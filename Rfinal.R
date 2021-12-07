wine <- read.csv("C:\\Users\\inha\\Desktop\\my-code\\data\\winequality-red.csv", header = T)
train_sub= sample(nrow(wine),7/10*nrow(wine))
train_data=wine[train_sub,]
test_data=wine[-train_sub,]
wine
class(wine)
library(tidyverse)
library(ggplot2)
library(mice)
library(VIM)
library(psych)
#####Univariate analysis of quality
summary(wine$quality)
ggplot(aes(x=quality),data=wine) + geom_bar()+scale_x_continuous(lim=c(3,9),breaks = seq(3,9,1))
summary(wine$fixed.acidity)
ggplot(aes(x=fixed.acidity),data=wine) + geom_bar()+scale_x_continuous(lim=c(4,16),breaks = seq(0,16,1))
summary(wine$volatile.acidity)
ggplot(aes(x=volatile.acidity),data=wine) + geom_bar()+scale_x_continuous(lim=c(0.1,2),breaks = seq(0.1,1.6,1))
summary(wine$citric.acid)
ggplot(aes(x=citric.acid),data=wine) + geom_bar()+scale_x_continuous(lim=c(0,1),breaks = seq(0,1,0.05))
summary(wine$residual.sugar)
ggplot(aes(x=residual.sugar),data=wine) + geom_bar()+scale_x_continuous(lim=c(0,16),breaks = seq(0,16,1))
summary(wine$chlorides)
ggplot(aes(x=chlorides),data=wine) + geom_bar()+scale_x_continuous(lim=c(0,0.7),breaks = seq(0,0.7,0.05))
summary(wine$free.sulfur.dioxide)
ggplot(aes(x=free.sulfur.dioxide),data=wine) + geom_bar()+scale_x_continuous(lim=c(1,80),breaks = seq(1,80,5))
summary(wine$total.sulfur.dioxide)
ggplot(aes(x=total.sulfur.dioxide),data=wine) + geom_bar()+scale_x_continuous(lim=c(0,300),breaks = seq(0,300,10))
summary(wine$density)
ggplot(aes(x=density),data=wine) + geom_histogram(binwidth=0.001)+scale_x_continuous(lim=c(0,1),breaks = seq(0,1,0.001))
summary(wine$pH)
ggplot(aes(x=pH),data=wine) + geom_bar()+scale_x_continuous(lim=c(2,4),breaks = seq(2,4,0.1))
summary(wine$sulphates)
ggplot(aes(x=sulphates),data=wine) + geom_bar()+scale_x_continuous(lim=c(0,2),breaks = seq(0,2,0.1))
summary(wine$alcohol)
ggplot(aes(x=alcohol),data=wine) + geom_bar()+scale_x_continuous(lim=c(8,15),breaks = seq(8,15,0.5))
summary(wine)

##########################Bivariate analysis
cor(wine)
wine1 <- cor(wine)
library(corrplot)
corrplot(wine1)


boxplot<- function(feature) {
  ggplot(data=wine, aes_string(x = "factor(quality)", y = feature)) +
    geom_jitter(alpha=0.3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", geom = "point", color = "red")+
    geom_smooth(method='lm', aes(group = 1))
}
boxplot("volatile.acidity")+scale_y_continuous(breaks = seq(0, 1.6, .1))
boxplot("citric.acid") +scale_y_continuous(breaks = seq(0, 1, .05))
boxplot("sulphates") +scale_y_continuous(breaks = seq(0, 2, .1))
boxplot("alcohol") +scale_y_continuous(breaks = seq(0, 15, 1))


####################################Multivariate correlation analysis

pairs.panels(wine[c("quality","alcohol","sulphates","density")])    #Draw a scatter plot matrix
pairs.panels(wine[c("quality","alcohol","sulphates","density","residual.sugar","pH","citric.acid")])   

##########
library(pROC)
# data preprocessing
train_data$level = as.numeric(train_data$level)
test_data$level= as.numeric(test_data$level)

############################################
library(e1071)
model=svm(level~.,data=wine)                #SVR regression
summary(model)

###################################################eps-regression ########################
##############kernel= radial basis###

library(e1071)
wine_svm<- svm (level ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density +  pH + sulphates + alcohol,
                scale = FALSE,
                data=train_data, 
                type='eps-regression',
                kernel='radial')
#test
pre_svm <- predict(wine_svm,newdata = test_data)
obs_p_svm = data.frame(prob=pre_svm,obs=test_data$level)
#output confusionMatrix
table(test_data$level,pre_svm,dnn=c("real value","predictive value"))
#plot ROC
svm_roc <- roc(test_data$level,as.numeric(pre_svm))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM ROC curve kernel = radial')


#############kernel= polynomial###
wine_svm<- svm(level ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=train_data, type='eps-regression',kernel='polynomial')
#test
pre_svm <- predict(wine_svm,newdata = test_data)
obs_p_svm = data.frame(prob=pre_svm,obs=test_data$level)
#output confusionMatrix
table(test_data$level,pre_svm,dnn=c("real value","predictive value"))
#plot ROC
svm_roc <- roc(test_data$level,as.numeric(pre_svm))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM ROC curve kernel = polynomial')


############kernel= linear ###
wine_svm<- svm(level ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=train_data, type='eps-regression',kernel='linear')
#test
pre_svm <- predict(wine_svm,newdata = test_data)
obs_p_svm = data.frame(prob=pre_svm,obs=test_data$level)
#output confusionMatrix
table(test_data$level,pre_svm,dnn=c("real value","predictive value"))
#plot ROC
svm_roc <- roc(test_data$level,as.numeric(pre_svm))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM ROC curve kernel = linear')


###########kernel= sigmoid ###
wine_svm<- svm(level ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=train_data, type='eps-regression',kernel='sigmoid')
#test
pre_svm <- predict(wine_svm,newdata = test_data)
obs_p_svm = data.frame(prob=pre_svm,obs=test_data$level)
#output confusionMatrix
table(test_data$level,pre_svm,dnn=c("real value","predictive value"))
#plot ROC
svm_roc <- roc(test_data$level,as.numeric(pre_svm))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM ROC curve kernel = sigmoid')

######################################################################3

lm(formula=quality~ volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol,data=wine)

###########################################chose  best model
attach(wine)
x=subset(wine,select=-level)
y=level
type=c("C-classification","nu-classification","one-classification","eps-regression","nu-regression")
kernel=c("linear","polynomial","radial","sigmoid")
pred=array(0,dim=c(1599,5,4))
accuracy=matrix(0,5,4)
yy=as.integer(y)
library(e1071)
for(i in 1:5)
{
  for (j in 1:4) 
  {
    pred[,i,j]=predict(svm(x,y,type=type[i],kernel=kernel[j]),x)
    if(i>2) accuracy[i,j]=sum(pred[,i,j]!=1)
    else accuracy[i,j]=sum(pred[,i,j]!=yy)
  }
}
dimnames(accuracy)=list(type,kernel)
accuracy

######################################################

svr.model <- svm(fixed.acidity~ level, data =wine, type = "eps-regression",kernel = "radial")
summary(svr.model)
#library(ggplot2)
#ggplot()+
# geom_point(aes(wine$fixed.acidity,wine$level), color = "blue")+
#geom_line(aes(wine$level, predict(svr.model, wine)), color = "red")

#pred <- predict(svr.model, data.frame(level = 1))
#pred