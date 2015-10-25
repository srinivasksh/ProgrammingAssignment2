## This project is about calculating inverse of an input
## matrix. However, to avoid same calculations everytime,
## the output is cached into a dataframe (if one does not exist)


## Returns a special vector of various details pertaining to input matrix
## This is cached to be further used by cacheSolve function
makeCacheMatrix <- function(x = numeric()) {
    ## Initialize Inv variable
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    
    ## calcInv column variable
    calcInv <- function(solve) Inv <<- solve
    
    ## getInv column variable
    getInv <- function() Inv
    
    ## create a list of set, get, calcInv, and getInv varables
    list(set = set, get = get, 
         calcInv = calcInv, 
         getInv = getInv)
}


## Returns the inverse of input matrix. The input matrix data
## is however given as input in the form of special vector (generated 
## in the makeCachematrix function)
cacheSolve <- function(x, ...) {
    
    ## Assign getInv column of dataframe : cached data
    Inv <- x$getInv()
    
    ## Check if cached variable is already available
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
    ## get the matrix data
    data <- x$get()
    
    ## calculate Inverse of the input matrix
    Inv <- solve(data, ...)
    
    ## cache the inverse to x dataframe
    x$calcInv(Inv)
    
    ## return the Inverse
    Inv
}