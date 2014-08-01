/* UPC shared variable %qE is declared
   both strict and relaxed.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared strict relaxed int x; /* { dg-error "UPC shared variable 'x' is declared both strict and relaxed" } */
