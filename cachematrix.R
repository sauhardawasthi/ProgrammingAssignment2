
## My R programming course Assignemnt 2

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## defining the argument with the default mode of matrix
    inv <- NULL                             ## initializing inv as NULL as it'll hold matrix inverse's value
    set <- function(y) {                    ## defining the set function to assign new 
        x <<- y                             ## matrix value in parent environment
        inv <<- NULL                        ## if there is a new matrix, resetting inv to NULL
    }
    get <- function() x                     ## defining the get function here

    setinverse <- function(inverse) inv <<- inverse  ## assigning inv's value in parent environment
    getinverse <- function() inv                     ## getting inv's value where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and matrix hasn't changed), then cacheSolve retrieves innverse from cache.
cacheSolve <- function(x, ...) {
        ## Returning matrix that's inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
