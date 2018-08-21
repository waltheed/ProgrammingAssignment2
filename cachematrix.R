# Programming Assignment 2: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# This is a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) 
#  This function creates a special "matrix" object that can cache its inverse.
{   
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) 
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache. 
{
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

## Code to test...

# m <- matrix(c(1,2,3,4),2,2)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)

