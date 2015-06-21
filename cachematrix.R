## This file contains functions, makeCacheMatrix() and cahceSolve() 
## that help to compute and cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        ## Functions to get and set the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        ## Functions to get and set inverse of the matrix
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check if inverse has already been cached
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## Compute the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        
        ## Cache the inverse
        x$setinverse(inverse)
        
        ## Return Inverse
        inverse
}
