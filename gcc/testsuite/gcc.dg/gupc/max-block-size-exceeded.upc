/* Maximum UPC block size in this implementation exceeded.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [UPC_MAX_BLOCK_SIZE+1] int A[(UPC_MAX_BLOCK_SIZE+1)*THREADS]; /* { dg-error "the maximum UPC block size in this implementation is 1048575" } */
