setwd("E:/ll-项目/01-31 黄陕州 IHC")
a<-read.csv("组织芯片评分.csv", header = TRUE, sep = ",")
b<-a[grep(pattern = "Hep3b(.*)Caspase3",a[,1]),]
for(i in 1:nrow(b)){
  if(grepl(pattern = "000",a[i,1])) {
    b$group[i]<-"control"
  }else{
    b$group[i]<-"Hep3b-Caspase3"
  }
}
c<-t.test(b[which(b$group == "control"),4],b[which(b$group== "Hep3b-Caspase3"),4],paired = FALSE)
m<-c$estimate
names(m)<-c("control","Hep3b-Caspase3")
se= c(sd(b[which(b$group == "control"),4])/sqrt(length(b[which(b$group == "control"),4])), sd(b[which(b$group== "Hep3b-Caspase3"),4])/sqrt(length(b[which(b$group== "Hep3b-Caspase3"),4])))
windows()
bp = barplot(m, ylim=c(0, 7), xpd=FALSE)
box()
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)
text(labels="p=c$p.value")



nonsmokers <- c(18,22,21,17,20,17,23,20,22,21)
smokers <- c(16,20,14,21,20,18,13,15,17,21)
m        = c(mean(nonsmokers), mean(smokers))
names(m) = c("nonsmokers", "smokers")
se       = c(sd(nonsmokers)/sqrt(length(nonsmokers)), sd(smokers)/sqrt(length(smokers)))
windows()
bp = barplot(m, ylim=c(16, 21), xpd=FALSE)
box()
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)
text(x=1.3,y=20.5,cex=3,labels="**")

set.seed(4643)
plot(jitter(rep(c(0,1), each=10)), c(nonsmokers, smokers), axes=FALSE, xlim=c(-.5, 1.5),
     xlab="", ylab="")
box()
axis(side=1, at=0:1, labels=c("nonsmokers", "smokers"))
axis(side=2, at=seq(14,22,2))
points(c(0,1), m, pch=15, col="red")
arrows(x0=c(0,1), y0=m-se, y1=m+se, code=3, angle=90, length=.15)

