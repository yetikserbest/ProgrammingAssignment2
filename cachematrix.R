## There are three functions in this file
## 1. makeCacheMatrix 
## It is a first class function which sets
## the inverse of a matrix of the object to NULL, and it creates 
## four functions for the object: get (for getting the matrix), 
## set (for setting the matrix), getinv (for getting the inverse of
## the matrix, and setinv (for setting the inverse of the matrix).
## It uses "<<-" operator to make the changes effective to the parent  
## environment (the one calling this function). It returns a list with
## these functions.
## 2. cacheSolve 
## It is a fucntion that utilizes the makeCacheMatrix functions to
## calculalte the inverse of a matrix. If the inverse of the matrix
## stored in invMatrix varibale in the parent environment is NULL, then 
## it calculates the inverse by utilizing solve function. If it is not
## NULL, then that means the inverse of that particular matrix is already 
## calculated and is already cached in invMatrix variable, and it returns
## that.
## 3. testCacheMatrix
## This function is created to etst caching of inverse of a matrix. 
## That is, it tests the previous two functions above. It creates three
## square matrices, and by using makeCacheMatrix and cacheSolve functions
## it tests taking inverse of a matrix, then it tests if matrix of an
## object can be set, then it tests if taking inverse of teh same matrix
## works directly from the cache. It repeats these steps for all three 
## matrices created.
## 
##
## ######## makeCacheMatrix ###########
## It is a first class function which sets the inverse of a matrix of 
## the object to NULL, and it creates four functions for the object: 
## 1. get (for getting the matrix), 
## 2. set (for setting the matrix), 
## 3. getinv (for getting the inverse of the matrix, 
## 4. setinv (for setting the inverse of the matrix).
## It uses "<<-" operator to make the changes effective to the parent  
## environment (the one calling this function). It returns a list with
## these functions.

makeCacheMatrix <- function(x = matrix()) {
	  ## set the inverse of the matrix to NULL when teh object is
	  ## first created
        invMatrix <- NULL
	  ## set function to set the matrix of the object
        ## and since that also means thsi is a new matrix,
	  ## hence the inverse of teh matrix is also set to NULL 
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
	  ## gets and returns the matrix of the object
        get <- function() x
	  ## sets the inverse of the matrix with the provided inverse matrix
        setinv <- function(inv) invMatrix <<- inv
	  ## gets and returns the inverse matrix of the object's matrix
        getinv <- function() invMatrix
 	  ## returns the functions as a list as this is the frist-class fucntion
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


########### cacheSolve ########### 
## It is a fucntion that utilizes the makeCacheMatrix functions to
## calculate the inverse of a matrix. If the inverse of the matrix
## stored in invMatrix varibale in the parent environment is NULL, then 
## it calculates the inverse by utilizing R solve function. If it is not
## NULL, then that means the inverse of that particular matrix is already 
## calculated and is already cached in invMatrix variable, and it returns
## that.

cacheSolve <- function(x, ...) {
        ## first, it queries the matrix's inverse cache
        invMatrix <- x$getinv()
        ## if there is a cached inverse matrix for the matrix of the object	
        if(!is.null(invMatrix)) {
		    ## if tehre is cached inverse matrix it returns that one
		    ## without calculation
                message("getting cached inverse Matrix")
                return(invMatrix)
        }
	  ## If there is no cached inverse matrix for this object
	  ## it gest the matrix of the object first
        data <- x$get()
	  ## it calculates the inverse by R solve function
        ## and stores it in objects invMatrix variable
        invMatrix <- solve(data, ...)
	  ## It sets the inverse matrix of the object with the calculated one
        x$setinv(invMatrix)
	  ## returns the calculted one rather than the cached one, whci doesn't
	  ## exists
        invMatrix
}

########### testCacheMatrix ########### 
## This is a ready-made function to test the inverse cahaching functions
## it creates 3 square matrices, taeks inverse of each one twice to test
## if caching of inverse matrix works

testCacheMatrix <- function() {
	## crete three square matrices
	x1=matrix(c(1,2,3,6,0,4,7,8,9),3,3)
	x2=matrix(c(11,12,13,16,10,14,17,18,19),3,3)
	x3=matrix(c(21,22,23,26,20,24,27,28,29),3,3)
	## creat cache matrix fro the frist one
	b<-makeCacheMatrix(x1)
	## display the matrix by testing get function
	print("x1 matrix")
	print(b$get())
	## calculate inverse of the matrix
	print("inverse of x1")
	print(cacheSolve(b))
	## get the inverse of the same matrix from the cache
	print("inverse of x1 from cache")
	print(cacheSolve(b))
	## change the matrix by testing set function
	print("the same object with matrix x2")
	b$set(x2)
	print("x2 matrix")
	print(b$get())
	## calculate inverse of the matrix
      print("inverse of x2")
	print(cacheSolve(b))
	## get the inverse of the same matrix from the cache
	print("inverse of x2 from cache")
	print(cacheSolve(b))
	## change the matrix by testing set function
	print("a new object with matrix x3")
	b<-makeCacheMatrix(x3)
	print("x3 matrix")
	print(b$get())
	## calculate inverse of the matrix
      print("inverse of x3")
	print(cacheSolve(b))
	## get the inverse of the same matrix from the cache
	print("inverse of x3 from cache")
	print(cacheSolve(b))

}
