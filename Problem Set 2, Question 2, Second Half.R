library(MASS)
data() 
data(Cars93,package="MASS")
head(Cars93)
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + Width + Length + Weight + Price + Cylinders + Horsepower + Wheelbase,data=Cars93)
plot(treeCars)
text(treeCars)
par(mfrow=c(1,2))
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass"))
plot(cv.tree(treeCars))
pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)
table(Cars93$DriveTrain,predict(pruneTree,type="class"))
table(Cars93$DriveTrain,predict(treeCars,type="class"))

