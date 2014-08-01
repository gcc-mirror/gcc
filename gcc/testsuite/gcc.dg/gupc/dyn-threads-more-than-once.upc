/* In the UPC dynamic translation environment,
   THREADS must appear exactly once in 
   declarations of shared arrays; 
   the storage size cannot be calculated.
	{ dg-do compile }
	{ dg-options "-fupc-threads=0" } */

extern int THREADS;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int A[THREADS*THREADS]; /* { dg-error "UPC shared array declaration references THREADS more than once; the size of 'A' cannot be calculated" } */
