## Author: Rafael Odon (odon.rafael@gmail.com)
## Date: 2014-06-13
##
## Based on the Assigment mean example I've created a first function that can 
## store and retrieve a matrix and its inverse, and it acts like a cache object. 
## Anonther function uses the first one then, asking for the cached inverse. 
## If it already exists, it is retrieved. Else, it stores the inverse in there.
##
## Example of usage:
## > source("cachematrix.R")
## > m <- matrix(rnorm(16), 4, 4)
## > m2 <- makeCacheMatrix(m)
## > cacheSolve(m2) # prints the inverse matrix
## > cacheSolve(m2) # should output "getting cached data" after printing the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	## This function stores a matrix and its inverse for cache purposes
	##
	## Use the set/get to store/retrieve the matrix
	## Use the setInverseMatrix/getInverseMatrix to store/retrieve its inverse

	inverseMatrix <- NULL

	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}

	get <- function() {
		x
	}

	setInverseMatrix <- function(outterInverseMatrix) {
		inverseMatrix <<- outterInverseMatrix
	}
	
	getInverseMatrix <- function() {
		inverseMatrix
	}

	list(set = set, get = get, 
		setInverseMatrix = setInverseMatrix,
		getInverseMatrix = getInverseMatrix)
}


cacheSolve <- function(x, ...) {
        ## This function returns a matrix that is the inverse of 'x'
	##
	## Once the inverse is calculated it is stored in a cache in order to avoid 
	## calculating the inverse again in later calls
	
	inverse <- x$getInverseMatrix()
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setInverseMatrix(inverse)
	inverse
}
