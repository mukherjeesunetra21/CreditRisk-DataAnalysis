#dataset loading
rawdata=read.csv("D:/project/kc/Credit_Risk_Train_data.csv",na.strings = c("","NA"))
rawdata2=read.csv("D:/project/kc/Credit_Risk_Validate_data.csv",na.strings = c("","NA")) 
library(dplyr)
data=rbind(rawdata,rawdata2)
dim(data)
dim(rawdata2)
str(data)
data %>% is.na() %>% colSums()

library(VIM)
#Aggrigation Plot
data %>% aggr(combined=TRUE,numbers=TRUE,cex.axis=.5,cex.numbers=.5,
              labels=names(rawdata),ylab='Missing data patterns')
#using naniar
library(naniar)
naniar::gg_miss_var(data)

#deleating rows with NA
data=na.omit(data)
data %>% is.na() %>% colSums()
dim(data)

#splitting data into train and test data
samplesize=floor(0.75*nrow(data))
set.seed(123)
traind=sample(seq_len(nrow(data)),size=samplesize)
train=data[traind,]
test=data[-traind,]
head(train)
library(pxR)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(rayshader)
library(knitr)
library(magrittr)
library(ggplot2)
library(plotly)
library(gganimate)
library(av)
library(gifski)


static=plot_ly(train,x=~ApplicantIncome,y=~CoapplicantIncome,z=~LoanAmount)%>%add_markers(symbol=~Loan_Status,size=~LoanAmount+ApplicantIncome+CoapplicantIncome,jitter=0.7)

ggplotly(static)

#sub pot
p1=plot_ly(train,x=~ApplicantIncome,y=~CoapplicantIncome)%>%add_markers(symbol=~Loan_Status,marker=list(opacity=0.5))%>%layout(xaxis=list(range=c(0,20000)),yaxis=list(range=c(0,10000)))
p2=plot_ly(train,x=~ApplicantIncome,y=~LoanAmount)%>%add_markers(symbol=~Loan_Status,marker=list(opacity=0.5))%>%layout(xaxis=list(range=c(0,20000)),yaxis=list(range=c(0,650)))
p3=plot_ly(train,x=~LoanAmount,y=~CoapplicantIncome)%>%add_markers(symbol=~Loan_Status,marker=list(opacity=0.5))%>%layout(xaxis=list(range=c(0,500)),yaxis=list(range=c(0,10000)))
z=subplot(p1,p2,p3,titleX = TRUE,titleY = TRUE)%>% highlight(on='plotly_selected',persistent = FALSE)
z


library(gganimate)
library(ggpubr)
l1=ggplot(train,aes(x=Married,fill=Loan_Status))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l2=ggplot(train,aes(x=Gender,fill=Loan_Status))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l3=ggplot(train,aes(x=Education,fill=Loan_Status))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l4=ggplot(train,aes(x=Self_Employed,fill=Loan_Status))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l5=ggplot(train,aes(x=Property_Area,fill=Loan_Status))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l6=ggplot(train,aes(x=as.factor(Credit_History),fill=Loan_Status))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))
fig=ggarrange(l1,l2,l3,l4,l5,l6,labels=c("A","B","c","D","E","F"),ncol=3,nrow=2)
fig



#boxplot
A=ggplot(train,aes(y=LoanAmount,x=Education,fill=Loan_Status))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
B=ggplot(train,aes(y=LoanAmount,x=Property_Area,fill=Loan_Status))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
C=ggplot(train,aes(y=LoanAmount,x=Gender,fill=Loan_Status))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
D=ggplot(train,aes(y=LoanAmount,x=Married,fill=Loan_Status))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
E=ggplot(train,aes(y=LoanAmount,x=Self_Employed,fill=Loan_Status))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
F=ggplot(train,aes(y=LoanAmount,x=as.factor(Credit_History),fill=Loan_Status))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
fig2=ggarrange(A,B,C,D,E,F,labels=c("A","B","c","D","E","F"),ncol=2,nrow=3)
fig2

A=ggplot(train,aes(y=CoapplicantIncome,x=Education,fill=Loan_Status))+stat_boxplot(geom='errorbar')+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
B=ggplot(train,aes(y=CoapplicantIncome,x=Property_Area,fill=Loan_Status))+stat_boxplot(geom='errorbar')+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
C=ggplot(train,aes(y=CoapplicantIncome,x=Gender,fill=Loan_Status))+stat_boxplot(geom='errorbar')+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
D=ggplot(train,aes(y=CoapplicantIncome,x=Married,fill=Loan_Status))+stat_boxplot(geom='errorbar')+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
E=ggplot(train,aes(y=CoapplicantIncome,x=Self_Employed,fill=Loan_Status))+stat_boxplot(geom='errorbar')+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
F=ggplot(train,aes(y=CoapplicantIncome,x=as.factor(Credit_History),fill=Loan_Status))+stat_boxplot(geom='errorbar')+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
fig4=ggarrange(A,B,C,D,E,F,labels=c("A","B","c","D","E","F"),ncol=2,nrow=3)
fig4










# violin plot keeping Applicantmoney
A1=ggplot(train,aes(y=ApplicantIncome,x=Education,fill=Loan_Status))+
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))+theme(axis.text.x=element_text(angle = 90,hjust=1))
B1=ggplot(train,aes(y=ApplicantIncome,x=Property_Area,fill=Loan_Status))+
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))+theme(axis.text.x=element_text(angle = 90,hjust=1))
C1=ggplot(train,aes(y=ApplicantIncome,x=Gender,fill=Loan_Status))+
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))+theme(axis.text.x=element_text(angle = 90,hjust=1))
D1=ggplot(train,aes(y=ApplicantIncome,x=Married,fill=Loan_Status))+
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))+theme(axis.text.x=element_text(angle = 90,hjust=1))
E1=ggplot(train,aes(y=ApplicantIncome,x=Self_Employed,fill=Loan_Status))+
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))+theme(axis.text.x=element_text(angle = 90,hjust=1))
F1=ggplot(train,aes(y=ApplicantIncome,x=as.factor(Credit_History),fill=Loan_Status))+
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))+theme(axis.text.x=element_text(angle = 90,hjust=1))
fig3=ggarrange(A1,B1,C1,D1,E1,F1,labels=c("A","B","c","D","E","F"),ncol=2,nrow=3)
fig3






library(corrplot)
coodata=train%>% select(ApplicantIncome,CoapplicantIncome,LoanAmount)
head(coodata)
M=cor(coodata)
head(round(M,2))

#Style1
corrplot(M,method='circle',title='circle style',mar=c(0,0,1,0))
#style2
corrplot(M,method='pie',title='pie style',mar=c(0,0,1,0))
#stype3
corrplot(M,method='color',title='color style',mar=c(0,0,1,0))
#style4
corrplot(M,method='number',title='number style',mar=c(0,0,1,0))
#style5
corrplot(M,type='upper',title='upper style',mar=c(0,0,1,0))





library(sjPlot)

library(ggmosaic)
library(reprex)
library(gridExtra)
library(data.table)
#mosac
M1=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Gender),fill=Loan_Status),na.rm=TRUE)+labs(x='Gender(Female,Male)',y='Loan_Status')
tab1=train%>%select(Gender,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Gender","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M2=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Married),fill=Loan_Status),na.rm=TRUE)+labs(x='Married(N0,Yes)',y='Loan_Status')
tab2=train%>%select(Married,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Married","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M3=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Dependents),fill=Loan_Status),na.rm=TRUE)+labs(x='Dependents(0,1,2,3+)',y='Loan_Status')
tab3=train%>%select(Dependents,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Dependents","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M4=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Education),fill=Loan_Status),na.rm=TRUE)+labs(x='Education(Graduation,Non-Graduation)',y='Loan_Status')
tab4=train%>%select(Education,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Education","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M5=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Self_Employed),fill=Loan_Status),na.rm=TRUE)+labs(x='SelfEnployed(No.yes)',y='Loan_Status')
tab5=train%>%select(Self_Employed,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Self_Employed","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M6=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Property_Area),fill=Loan_Status),na.rm=TRUE)+labs(x='Propertyarea(rural,semiurban,urban',y='Loan_Status')
tab6=train%>%select(Property_Area,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Property_Area","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M7=ggplot(train)+geom_mosaic(aes(x=product(Loan_Status,Credit_History),fill=Loan_Status),na.rm=TRUE)+labs(x='Credit_History(0,1)',y='Loan_Status')
tab7=train%>%select(Credit_History,Loan_Status)%>%sjtab(fun='xtab',var.labels=c("Credit_History","Loan_Status"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)

M1

A=ggarrange(M1,M2)
tab1
B=ggarrange(M3,M4)
C=ggarrange(M5,M6,M7)

library(dplyr)
library(class)


unique(train$Loan_Amount_Term)

train%>%select(Loan_Amount_Term)%>%mutate(Loan_Amount_Term=case_when(Loan_Amount_Term<=300~"<=300",Loan_Amount_Term>300~"upto480"))


#main
train=train%>%select(Gender,Married,Dependents,Education,ApplicantIncome,CoapplicantIncome,LoanAmount,Credit_History,Property_Area,Loan_Status,Loan_Amount_Term)%>%mutate(Loan_Amount_Term=case_when(Loan_Amount_Term<=300~"<=300",Loan_Amount_Term>300~"upto480"))%>%mutate_at(vars(Gender,Married,Dependents,Education,Credit_History,Property_Area,Loan_Status,Loan_Amount_Term),list(factor))

library(broom)
#checking new loan status levels
train%>%group_by(Loan_Amount_Term)%>%summarise(no_roes=length(Loan_Amount_Term))






#experiment
null_m=glm(Loan_Status~1,data=train,family = 'binomial')
full_m=glm(Loan_Status~.,data=train,family = 'binomial')
step_model=step(null_m,scope = list(lower=null_m,upper=full_m),direction = 'forward')














#mainmodel
ml=glm(Loan_Status~Gender+Married+Dependents+Education+Loan_Amount_Term+Credit_History+Property_Area+ApplicantIncome+CoapplicantIncome+LoanAmount,data=train,family=binomial)
glance(ml)$AIC
broom::glance(ml)
summary(ml)






#excluding coaaplicantincome
ml1=glm(Loan_Status~Gender+Married+Dependents+Education+Loan_Amount_Term+Credit_History+Property_Area+ApplicantIncome+LoanAmount,data=train,family=binomial)
summary(ml1)

#excluding loan amount term
ml2=glm(Loan_Status~Gender+Married+Dependents+Education+Credit_History+Property_Area+ApplicantIncome+LoanAmount,data=train,family=binomial)
summary(ml2)





#excluding dependents
ml3=glm(Loan_Status~Gender+Married+Education+Credit_History+Property_Area+ApplicantIncome+LoanAmount,data=train,family=binomial)
summary(ml3)


#excluding educatiion
ml4=glm(Loan_Status~Gender+Married+Credit_History+Property_Area+ApplicantIncome+LoanAmount,data=train,family=binomial)
summary(ml4)


#excluding loanamount
ml5=glm(Loan_Status~Gender+Married+Credit_History+Property_Area+LoanAmount,data=train,family=binomial)
summary(ml5)



#excluding gender
ml6=glm(Loan_Status~Married+Credit_History+Property_Area+LoanAmount,data=train,family=binomial)
summary(ml6)





#excluding loan amount

ml7=glm(Loan_Status~Married+Credit_History+Property_Area,data=train,family=binomial)
summary(ml7)


#excluding property_Area
ml8=glm(Loan_Status~Married+Credit_History,data=train,family=binomial)
summary(ml8)



ml9=glm(Loan_Status~Married+Credit_History+Property_Area+ApplicantIncome,data=train,family=binomial())
summary(ml9)





#deviance residuals
model.data=augment(ml6)
R=data.frame(residuals(ml6))
ggplot(R,aes(x=as.numeric(row.names(model.data)),residuals.ml6.))+geom_point(aes(color=train$Loan_Status),alpha=0.5)+theme_bw()+ylim(-3,3)+geom_line(y=3)+geom_line(y=-3)+labs(y="Deviance Residuals",x="index")+ggtitle("ml6")
R1=data.frame(residuals(ml7))
model.data=augment(ml7)
ggplot(R1,aes(x=as.numeric(row.names(model.data)),residuals.ml7.))+geom_point(aes(color=train$Loan_Status),alpha=0.5)+theme_bw()+ylim(-3,3)+geom_line(y=3)+geom_line(y=-3)+labs(y="Deviance Residuals",x="index")+ggtitle("ml7")
R3=data.frame(residuals(ml9))
model.data=augment(ml9)
ggplot(R3,aes(x=as.numeric(row.names(model.data)),residuals.ml9.))+geom_point(aes(color=train$Loan_Status),alpha=0.5)+theme_bw()+ylim(-3,3)+geom_line(y=3)+geom_line(y=-3)+labs(y="Deviance Residuals",x="index")+ggtitle("ml9")

#fitted vs residuals graph
fit=data.frame(fitted(ml6))
L=fit%>%select(fitted.ml6.)%>%mutate(deviance_Residuals=R$residuals.ml6.)
ggplot(L,aes(x=fitted.ml6.,y=deviance_Residuals))+geom_point(aes(color=train$Loan_Status),alpha=0.5)+theme_bw()+ggtitle("ml6")


fit=data.frame(fitted(ml7))
L1=fit%>%select(fitted.ml7.)%>%mutate(deviance_Residuals=R1$residuals.ml7.)
ggplot(L1,aes(x=fitted.ml7.,y=deviance_Residuals))+geom_point(aes(color=train$Loan_Status),alpha=0.9,position = "jitter")+theme_bw()+ggtitle("ml7")




fit=data.frame(fitted(ml9))
L3=fit%>%select(fitted.ml9.)%>%mutate(deviance_Residuals=R3$residuals.ml9.)
ggplot(L3,aes(x=fitted.ml9.,y=deviance_Residuals))+geom_point(aes(color=train$Loan_Status),alpha=0.9,position = "jitter")+theme_bw()+ggtitle("ml9")

#good nedd of fit
#for ml6
ml6fit=fitted(ml6)
library(ResourceSelection)
train$Loan_Status[train$Loan_Status=="Y"]=1
train$Loan_Status[train$Loan_Status=="N"]=0

r1=hoslem.test(as.numeric(train$Loan_Status),ml6fit,g=32)




ml7fit=fitted(ml7)
r2=hoslem.test(as.numeric(train$Loan_Status),ml7fit,g=32)


ml9fit=fitted(ml9)
r3=hoslem.test(as.numeric(train$Loan_Status),ml9fit,g=32)

r1
r2
r3


#std perarson
ml6p=sum((as.numeric(train$Loan_Status)-ml6fit)^2/(ml6fit*(1-ml6fit)))
ml7p=sum((as.numeric(train$Loan_Status)-ml7fit)^2/(ml7fit*(1-ml7fit)))
ml9p=sum((as.numeric(train$Loan_Status)-ml9fit)^2/(ml7fit*(1-ml9fit)))
ml9p




#pred

test=test%>%select(Gender,Married,Dependents,Education,ApplicantIncome,CoapplicantIncome,LoanAmount,Credit_History,Property_Area,Loan_Status,Loan_Amount_Term)%>%mutate(Loan_Amount_Term=case_when(Loan_Amount_Term<=300~"<=300",Loan_Amount_Term>300~"upto480"))%>%mutate_at(vars(Gender,Married,Dependents,Education,Credit_History,Property_Area,Loan_Status,Loan_Amount_Term),list(factor))

library(Metrics)
library(pROC)
library(ggplot2)
pm6=predict(ml6,test,type='response')
pm6c=ifelse(pm6>=0.50,'Y','N')
x1=test%>%select(Loan_Status)%>%mutate(Loan_Status,prob=pm6c)
x1=x1%>%mutate_at(vars(prob),list(factor))
model.data=augment(ml6)
ggplot(model.data,aes(x=as.numeric(row.names(model.data)),.std.resid))+geom_point(aes(color=Loan_Status),alpha=0.5)+theme_bw()+ylim(-3,3)+geom_line(y=3)+geom_line(y=-3)
library(caret)
T=confusionMatrix(data=x1$prob,reference=x1$Loan_Status,positive=levels(x1$Loan_Status)[2],dnn=c("Referance","Prediction"))
str(x1)
T1=as.table(matrix(c(26,4,19,144),nrow = 2,byrow = TRUE))

library(pROC)
ROC1=roc(x1$Loan_Status,pm6,plot=TRUE,print.auc=TRUE,col='blue')
ce(x1$Loan_Status,x1$prob)


pm7=predict(ml7,test,type='response')
pm7c=ifelse(pm7>0.50,'Y','N')
x2=test%>%select(Loan_Status)%>%mutate(Loan_Status,prob=pm7c)
x2=x2%>%mutate_at(vars(prob),list(factor))
model.data=augment(ml7)
ggplot(model.data,aes(x=as.numeric(row.names(model.data)),.std.resid))+geom_point(aes(color=Loan_Status),alpha=0.5)+theme_bw()+geom_line(y=3)
library(caret)
T2=confusionMatrix(data=x2$prob,reference=x2$Loan_Status,positive=levels(x1$Loan_Status)[2])
str(x2)
Tb=as.table(matrix(c(25,4,20,144),nrow = 2,byrow = TRUE))

library(pROC)
ROC2=roc(x2$Loan_Status,pm7,plot=TRUE,print.auc=TRUE,col='green')
ce(x2$Loan_Status,x2$prob)

#comparison
plot(ROC1,col='blue',lty=7,main='Comparison btween 2 models')
plot(ROC2,col='red',add=TRUE,lty=1)


