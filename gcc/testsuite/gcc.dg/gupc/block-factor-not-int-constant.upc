/* UPC layout qualifier is not an integral constant.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [0.5] int A[10*THREADS]; /* { dg-error "UPC layout qualifier is not an integral constant" } */
