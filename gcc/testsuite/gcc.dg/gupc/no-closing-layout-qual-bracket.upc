/* Expected ] after UPC layout qualifier expression.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [5 int A[5 * THREADS]; /* { dg-error "expected \\\] before 'int'" } */

