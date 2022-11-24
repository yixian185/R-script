setwd('/Users/liyixian1990/Desktop/test2')

struc<-read.csv('c13biomarker.csv',row.names=1)
library(tidyverse)
library(randomForest)
struc2 <- struc %>%
  mutate(Structure = as.factor(Structure))
set.seed(100)
cera_rf= randomForest(Structure ~ ., data = struc2, importance=TRUE, proximity=TRUE)
cera_rf

imp_cera <- as_tibble(round(importance(cera_rf), 2),rownames = "Env") %>%
  arrange(desc(MeanDecreaseAccuracy))
imp_cera

ncol(struc2)
mycera= struc2[,-1]
set.seed(100)

result<-rfcv(mycera, struc2$Structure, cv.fold=10, scale = "log", step = 0.9)
result1<-result
result1$n.var#这一步就出结果了，找第一个最低的，和structure一样

with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

error.cv<-data.frame(num=result$n.var,error.1=result$error.cv)
for (i in 100:104){
  print(i)
  set.seed(i)
  result= rfcv(mycera, struc2$Structure, cv.fold=10, scale = "log", step = 0.9)
  error.cv = cbind(error.cv, result$error.cv)
}

n.var <- error.cv$num
error.cv <- error.cv[,-1]
colnames(error.cv)<- paste('err',1:5,sep='.')
err.mean <-apply(error.cv,1,mean)
allerr<-data.frame(num=n.var,err.mean=err.mean,error.cv)
head(allerr[,1:6])

optimal = 11#这里是要改动的，第一个最低点并不是14，而是4，但是5次模拟4并不是最低点，而后边的14才是最低点，所以选择14
main_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=1, colour="black"),
                   axis.line.y=element_line(size=1, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=12),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=12),
                   text=element_text(family="sans", size=12))
max(allerr$num)

ggplot() +
  geom_line(aes(x = allerr$num, y = allerr$err.1), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.2), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.3), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.4), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.5), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.mean), colour = 'black',size=1) +
  geom_vline(xintercept = optimal, colour='black', lwd=0.5, linetype=2) +
  coord_trans(x = "log2") +
  scale_x_continuous(breaks = c(1, 5, 10, 20, 30, 50)) + 
  labs( x='Number of Envs ', y='Cross-validation error rate') +
  annotate("text", x = optimal, y = max(allerr$err.mean), label=paste("Optimal = ", optimal, sep=""))+
  main_theme

result$n.var

write.csv(imp_cera[1:9,], "biomarks.csv")#这里取前19个，因为后边有负相关的，有0的数据图不太好看
library(ggsci)
library(RColorBrewer)
col<-brewer.pal(9,"YlGnBu")
col#造一个颜色板
imp_cera[1:8,] %>%
  select(Env,MeanDecreaseAccuracy) %>%
  arrange(MeanDecreaseAccuracy) %>%
  mutate(Env = forcats::fct_inorder(Env)) %>%
  ggplot(aes(x = Env, y = MeanDecreaseAccuracy))+

  geom_bar(aes(fill=Env),stat = "identity")+
  #scale_fill_lancet()+
  scale_fill_manual(values = col)+

  labs(x = "", y = "Mean decrease accuracy")+
  coord_flip()+

  main_theme ->p2
p2   