##Author: Marcin Szuberla
## 3/18/2015

##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than computing it repeatedly.
##Below are two functions that calculate an inverse of an inversible matrix and cache it,
##for later retrieval.


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    
    #matrix can only be inversed if it is square, complex or numeric
    inv <- NULL    #inverse matrix
        
    isSquare <- nrow(x) == ncol(x)
    isNumericORcomplex <- (is.numeric(x) | is.complex(x))
    isInversible <- (isSquare & isNumericORcomplex)

    if(isInversible)    #if matrix is invesrible
    {
        inv <- NULL
        set <- function(y)
        {
            x <<- y
            inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
    }
    
    else
    {
        ##Print error message
        print(paste("Matrix not inversible!"))
    }    
}




##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    inv <- x$getInverse()
    if(!is.null(inv)) 
        {
            message("Getting cached inversed matrix!")
            return(inv)
    }
    
    matrix <- x$get()
    matrix_inverse <- solve(matrix, ...)
    x$setInverse(matrix_inverse)    #set matrix inverse
    
    ## Return a matrix that is the inverse of 'x'
    matrix_inverse
}
