## This pair of functions cache the inverse of a matrix

## makeCacheMatrix: this function creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
## create the list containing the functions
	set <- function(y) {
			x    <<- y
			invx <<- NULL
	}

	get <- function() x
	setinv <- function(solve) invx <<- solve
	getinv <- function() invx
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}

## cacheSolve: this function computes the inverse of the special matrix
## object returned by makeCacheMatrix above. If the inverse has already 
## been calculated and the matrix has not been changed, then the function
## should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
	invx <- x$getinv()    #get the inverse
	if(!is.null(invx)) {  #if the inverse could be found, retrieve
		message("getting cached data")
		return(invx)    # and quit the function
	}
	data <- x$get()       
	invx <- solve(data)   # else, calculate the inverse
	x$setinv(invx)        # and cache it
	invx
}