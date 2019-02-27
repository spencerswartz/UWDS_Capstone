#using Igraph to generatecordinates
edge = read.csv("~/UWDS/785Capstone/BrewersAssociation/UWDS_Capstone/Data/retweetFollowers_edgelist.csv", header = TRUE, )

library(igraph)

el = graph_from_data_frame(edge)

plot(el, vertex.size = 1,vertex.label = NA, edge.arrow.size = .1)

edge2 = edge[edge$isUnique == 0,]
el2 = graph_from_data_frame(edge2)
plot(el2, vertex.size = 1,vertex.label = NA, edge.arrow.size = .1)

edge3 = edge[edge$Id %in% edge$Origin.id,]
el3 = graph_from_data_frame(edge3)
plot(el3, vertex.size = 3,vertex.label = NA, edge.arrow.size = .1)

allorigin = unique(edge$Origin.id)
allorigin3 = unique(edge3$Origin.id)

missing = setdiff(allorigin,allorigin3)
missing = data.frame(missing,missing)
names(missing) = c('Origin.id','Id')
missing$isUnique = NA
all = rbind(missing, edge3)
eall = graph_from_data_frame(all)
plot(eall, vertex.size = 3,vertex.label = NA, edge.arrow.size = .1)
simpeall = simplify(eall)
plot(simpeall, vertex.size = 3,vertex.label = NA, edge.arrow.size = .1, layout = layout.fruchterman.reingold)

xy<-data.frame(layout_with_fr(simpeall))
xy$user = V(simpeall)$name
write.csv(xy, "~/UWDS/785Capstone/BrewersAssociation/UWDS_Capstone/Data/retweetFollowers_xy.csv")

join = merge(edge,edge, by = 'Id', sort = TRUE)
#combos = unique(join[c('Origin.id.x','Origin.id.y')])

library(plyr)
combos = ddply(join,.(Origin.id.x,Origin.id.y),nrow)


todrop = c()
i = 1
for (x in 1:nrow(combos)) {
  if (combos[x, "Origin.id.x"] == combos[x, "Origin.id.y"]) {
    todrop[[i]] = x
    i =i+1
  }
}
combos = combos[-todrop, ]

combel = graph_from_data_frame(combos[which(combos$V1>1),])

plot(combel, vertex.size = 4,vertex.label = NA, edge.arrow.size = .1, layout = layout.fruchterman.reingold.grid)

xy<-data.frame(layout_with_fr(combel))
xy$user = V(combel)$name
write.csv(xy, "~/UWDS/785Capstone/BrewersAssociation/UWDS_Capstone/Data/retweetFollowers_xy.csv")
write.csv(combos[which(combos$V1>1),],"~/UWDS/785Capstone/BrewersAssociation/UWDS_Capstone/Data/retweetFollowers_common.csv")

library(data.table)
pairs = setDT(combos[which(combos$V1>1),], keep.rownames = TRUE)
out = merge(xy, pairs, by.x = 'user', by.y = 'Origin.id.x')
out2 = merge(xy, pairs, by.x = 'user', by.y = 'Origin.id.y')
write.csv(out, "~/UWDS/785Capstone/BrewersAssociation/UWDS_Capstone/Data/retweetFollowers_xyL.csv")
write.csv(out2, "~/UWDS/785Capstone/BrewersAssociation/UWDS_Capstone/Data/retweetFollowers_xyR.csv")
