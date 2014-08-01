/* Variable is declared with UPC relaxed qualifier
   but not shared.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



relaxed int x; /* { dg-error "'x' is declared with UPC relaxed qualifier but not shared" } */
