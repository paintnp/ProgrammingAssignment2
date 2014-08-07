## To cache the inverse of a matrix, we will first make a special type of matrix that can 
## store its own inverse. The first function does that. We pass a nornal matrix as an argument
## to the first function which returns a list of functions that can be used to 
## set the initial matrix, set the inverse of a matrix and to get the cached value. Initially, before the second function is called
## the inverse would be NULL

## Set up an invertible matrix to save its inverse upon the first time the function cacheSolve is called on the result of the function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)     
        
}



## cacheSolve first checks whether the argument (which is the returned list from the top function) cacheMatrix alredy has an inverse
## If so, it returns the inverse. Otherwise, it'll call solve() on the matrix assumed to be invertible and then save the state (the inverse)
## in the cacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
