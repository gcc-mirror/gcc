/* In the UPC dynamic translation environment,
   THREADS may not appear in declarations
   of shared arrays with indefinite block size;
   the storage size cannot be calculated.
	{ dg-do compile }
	{ dg-options "-fupc-threads=0" } */

extern int THREADS;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


shared [] int A[THREADS]; /* { dg-error "in the UPC dynamic translation environment, THREADS may not appear in declarations of shared arrays with indefinite block size; the storage size of 'A' cannot be calculated" } */
