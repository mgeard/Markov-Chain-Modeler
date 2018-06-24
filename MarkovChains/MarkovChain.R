require(markovchain)
require(igraph)

mat <- matrix(
	c(
		0.65, 0.25, 0.10,
		0.15, 0.65, 0.20,
		0.10, 0.40, 0.50
	),
	nrow = 3,
	byrow = TRUE
)

ourMarkovChain <- new(
	"markovchain",
	states = c("BA", "A", "AA"),
	transitionMatrix = mat
)

PlotMarkovChain <- function(markovChain, steps = 1) {

	stepStr <- "step"
	if (steps != 1) stepStr = "steps"

	markovChain <- markovChain ^ steps
	g <- as(markovChain, "igraph")
	
	plot(
		g,
		margin = 0.25,
		vertex.shape = "circle",
		vertex.size = 30,
		vertex.color = c("peachpuff4", "green4", "yellow4", "brown", "antiquewhite4", "red", "blue"), #rainbow(length(names(markovChain))),
		vertex.frame.color = NA,
		vertex.label.color = "white",
		vertex.label.font = 2,
		vertex.label.cex = 2,
		vertex.label.dist = 0,
	
		edge.width = E(g)$prob * 10,
		edge.arrow.size = 1.5,
		edge.arrow.width = 0.7,
		edge.curved = 0.2,
		edge.color = "antiquewhite3",
		edge.label = round(E(g)$prob, 5),
		edge.label.color = "brown",
		edge.label.cex = 1.5
	)
	title(paste("markov chain:", steps, stepStr), line = -3)

}

PlotMarkovChain(ourMarkovChain)