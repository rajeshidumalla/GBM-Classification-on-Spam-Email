# GBM Classification on Spam Email

Please read this README.md file to learn more about the GBM classification on Spam Email. Thanks 😊 

### Importing Libraries

```{r}
library(gbm)
library(knitr)
library(readxl)
library(tidyverse)
library(janitor)
library(rpart)
library(nnet)
library(scales)


knitr::opts_chunk$set(cache = TRUE,
warning = FALSE,
echo = TRUE,
message = FALSE,
dpi = 180,
fig.width = 6,
fig.height = 4)
theme_set(theme_classic())
```


### LOading Data

```{r}
spam_train <- read_csv("spam_train.csv",
col_names = FALSE)
spam_test <- read_csv("spam_test.csv",
col_names = FALSE)
rflabs<-c("make", "address", "all", "3d", "our", "over", "remove",
"internet","order", "mail", "receive", "will",
"people", "report", "addresses","free", "business",
"email", "you", "credit", "your", "font","000","money",
"hp", "hpl", "george", "650", "lab", "labs",
"telnet", "857", "data", "415", "85", "technology", "1999",
"parts","pm", "direct", "cs", "meeting", "original", "project",
"re","edu", "table", "conference", ";", "(", "[", "!", "$", "#",
"CAPAVE", "CAPMAX", "CAPTOT","type")
colnames(spam_train)<-rflabs
colnames(spam_test)<-rflabs
```

### GBM Model 1

```{r}
gbm0<-gbm(type~.,data=spam_train,train.fraction=1,interaction.depth=4,
shrinkage=.05,n.trees=200,bag.fraction=0.5,cv.folds=5,
distribution="bernoulli",verbose=T);
```

```{r}
gbm0.predict<-predict(gbm0,spam_test,type="response")
gbm0.predict_bin<-ifelse(gbm0.predict<0.5, 0, 1)
result<-ifelse(gbm0.predict_bin == spam_test$type,1,0)
err_rate = 1-mean(result)
```

```{r}
#print misclassification rate
print(err_rate*100)
temp1<-ifelse((spam_test$type==1) & (gbm0.predict_bin!=spam_test$type),1,0)
temp2<-ifelse(spam_test$type==1,1,0)
spam_err=sum(temp1)/sum(temp2)
print("spam misclassification rate")
print(spam_err*100)
temp1<-ifelse((spam_test$type==0) & (gbm0.predict_bin!=spam_test$type),1,0)
temp2<-ifelse(spam_test$type==0,1,0)
non_spam_err=sum(temp1)/sum(temp2)
print ("non-spam misclassification rate")
print(non_spam_err*100)
```

<img width="283" alt="Screen Shot 2021-10-10 at 1 48 07 pm" src="https://user-images.githubusercontent.com/56792400/136679579-0544db97-ccef-4ade-901a-e231cd2fd498.png">


### GBM Model 2

```{r}
w<-ifelse(spam_train$type==0,0.134,0.01)
gbm1<-gbm(type~.,data=spam_train,weights = w, train.fraction=1,
interaction.depth=4,shrinkage=.05, n.trees=200,bag.fraction=0.5,cv.folds=5,
distribution="bernoulli",verbose=T);
```


```{r}
gbm1.predict<-predict(gbm1,spam_test,type="response")
gbm1.predict_bin<-ifelse(gbm1.predict<0.5, 0, 1)
result<-ifelse(gbm1.predict_bin == spam_test$type,1,0)
err_rate = 1-mean(result)

#print misclassification rate

print ("ERROR rate")
print(err_rate*100)
temp1<-ifelse((spam_test$type==1) & (gbm1.predict_bin!=spam_test$type),1,0)
temp2<-ifelse(spam_test$type==1,1,0)
spam_err=sum(temp1)/sum(temp2)
print ("spam misclassification rate")
print(spam_err*100)
temp1<-ifelse((spam_test$type==0) & (gbm1.predict_bin!=spam_test$type),1,0)
temp2<-ifelse(spam_test$type==0,1,0)
non_spam_err=sum(temp1)/sum(temp2)
print ("non-spam misclassification rate")
print(non_spam_err*100)
```

<img width="509" alt="Screen Shot 2021-10-10 at 1 49 36 pm" src="https://user-images.githubusercontent.com/56792400/136679589-aba53b2e-5388-4a30-85bd-78daedbbb3ea.png">



















































