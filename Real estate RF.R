getwd()
setwd("F:\\datasets\\R LECTURES\\PROJECT\\Project1")
getwd()


#Imports
library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(car)

train=read.csv("housing_train.csv")
test=read.csv("housing_test.csv")

test$Price= NA

train$data = 'train'
test$data = 'test'


library(dplyr)

alldata= rbind(train,test)
View(alldata)

library(dplyr)

glimpse(alldata)

alldata=alldata %>% 
  select(-Suburb)

alldata=alldata %>% 
  select(-Address)

alldata=alldata %>% 
  select(- SellerG )
colnames(alldata)

table(alldata$Type)   

alldata=alldata %>% 
  mutate(Type_1=as.numeric(alldata$Type =='h'),
         Type_2=as.numeric(alldata$Type =='u')) %>% 
  select(-Type)

glimpse(alldata)


table(alldata$Method)

alldata=alldata %>% 
  mutate(m1=as.numeric(alldata$Method =='PI'),
         m2=as.numeric(alldata$Method =='S'),
         m3=as.numeric(alldata$Method =='SA'),
         m4=as.numeric(alldata$Method =='SP')) %>%
  select(-Method)

glimpse(alldata)

table(alldata$CouncilArea)
unique((alldata$CouncilArea))



alldata=alldata %>% 
  mutate(csa1=as.numeric(alldata$CouncilArea %in% c("Hume","Kingston","Monash", "Whitehorse","Manningham","Brimbank")),
         csa2=as.numeric(alldata$CouncilArea %in% c("Hobsons Bay", "Bayside","Melbourne", "Banyule")),
         csa3=as.numeric(alldata$CouncilArea %in% c("Port Phillip","Maribyrnong","Yarra")),
         csa4=as.numeric(alldata$CouncilArea %in% c("Stonnington", "Glen Eira")),
         csa5=as.numeric(alldata$CouncilArea %in% c("Moonee Valley","Darebin","Boroondara","Moreland")) ) %>%
  select(-CouncilArea)

glimpse(alldata)

View(alldata)

lapply(alldata,function(x) sum(is.na(x)))


for(col in names(alldata)){
  
  if(sum(is.na(alldata[,col]))>0 & !(col %in% c("data","price"))){
    
    alldata[is.na(alldata[,col]),col]=mean(alldata[,col],na.rm=T)
  }
  
}

View(alldata)

# Impute the NA values with Mean in the dataset

pr_train = alldata %>% filter(data == 'train') %>% select(-data) 
pr_test= alldata %>% filter(data == 'test') %>% select(-Price, -data) 

any(is.na(pr_train))
any(is.na(pr_test))

library(randomForest)
param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))

# For selecting Random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

library(cvTools)


myerror=9999999

for(i in 1:num_trials){
  
  params=my_params[i,]
  print(params)
  
  k=cvTuning(randomForest,Price~., 
             data =pr_train,
             tuning =params,
             folds = cvFolds(nrow(pr_train), K=10, type = "random"),
             seed =2
             )
  score.this=k$cv[,2]
 
 
  if(score.this<myerror){
    
    myerror=score.this
    print(myerror)
    
    best_params=params
    print(best_params)
    
    
  }
  
}

 pr.rf.final=randomForest(Price~.,
                         mtry=15,
                         ntree=50,
                         maxnodes=10,
                         nodesize=2,
                         data=pr_train)

test.pred=predict(pr.rf.final,newdata = pr_test)
write.csv(test.pred,"real-estate_RF.csv",row.names = F)


