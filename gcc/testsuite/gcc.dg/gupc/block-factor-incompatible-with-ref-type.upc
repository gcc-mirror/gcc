/* UPC layout qualifier is incompatible with the referenced type.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



typedef shared [3] int array_blocked_3_type[30];

shared [5] array_blocked_3_type A[THREADS]; /* { dg-error "UPC layout qualifier is incompatible with the referenced type" } */
