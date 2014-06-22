## We demonstrate caching result of matrix inverse in R

## makeCacheMatrix returns a special matrix with functions to
## - set/get its value
## - set/get its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is inverse of 'x'
## we use caching here to avoid repeated computations

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
