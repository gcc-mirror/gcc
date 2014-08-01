/* UPC shared array dimension is not a simple multiple 
   of THREADS; the size cannot be calculated.
	{ dg-do compile }
	{ dg-options "-fupc-threads=0" } */

extern int THREADS;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int A[10*THREADS + 1]; /* { dg-error "UPC shared array dimension is not a simple multiple of THREADS; the size of 'A' cannot be calculated." } */
