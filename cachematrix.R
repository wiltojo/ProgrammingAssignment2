## Multiple functions that cache the inverse of a matrix


## Create a matrix object that can cache the inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Inverse
    i <- NULL

    ## Set Matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get Matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Set inverse
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Get inverse
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute inverse of matrix, inverse has already been calc'ed. Retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Matrix inverse x
    m <- x$getInverse()

    ## Inverse if set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get matrix
    data <- x$get()

    ## Calc inverse
    m <- solve(data) %*% data

    ## Set inverse
    x$setInverse(m)

    ## Return matrix
    m
}
