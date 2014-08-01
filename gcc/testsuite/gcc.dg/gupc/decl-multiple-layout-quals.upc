/* Two or more layout qualifiers specified.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [10] shared [20] int x[200*THREADS]; /* { dg-error "two or more layout qualifiers specified" } */
