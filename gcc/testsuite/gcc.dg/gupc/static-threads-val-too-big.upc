/* THREADS value exceeds UPC implementation limit.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2147483647" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



/* Must be compiled with large static value of THREADS.  */
shared int A[THREADS];
/* { dg-excess-errors "THREADS value exceeds UPC implementation limit of 1024" } */
