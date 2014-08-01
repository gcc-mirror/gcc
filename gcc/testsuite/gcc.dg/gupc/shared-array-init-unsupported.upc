/* Initialization of UPC shared arrays is currently not supported.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int A[3*THREADS] = {1, 2, 3}; /* { dg-error "initialization of UPC shared arrays is currently not supported" } */
