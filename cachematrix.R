makeCachMatrix <- function(x = matrix()) {
        ##input is an invertible square matrix
        k <- NULL ## this matrix stores the results of inverse
        set <- function(y) {
                x <<- y  ## function environment
                k <<- NULL ## initialising inverse to null
        }
        get <- function() x ## function environment
        setinv <- function(inv) k <<- inv ##we are setting the inverse of the matrix
        getinv <- function() k ##gives the inverse matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

cachesolve <- function(x, ...) {
        ##function defines the inverse of the matrix x
        m <- x$getinv() ##gives the invese of x which will be null 
        ##for uncalculated inverse
        if(!is.null(m)) {
              ## if inverse has been already calculated   
                message("getting cached data")
                return(m) ##provide the inverse
        }
        data <- x$get() ## to get the matrix
        m <- inv(data, ...) ## give the inverse of data matrix
        x$setinv(m)
        return(m)
}

test<-matrix(sample.int(4, 100, TRUE), 4, 4) ##example of a matrix for testing
solve(test) ##give the inverse of our example matrix test
testCached <- makeCacheMatrix(test) ##making our invertible input matrix as test
testinv <- cacheSolve(testCached)## to compare our function with our example
