/* UPC [*] qualifier may not be used in declaration of pointers.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [*] int *pts; /* { dg-error "UPC \\\[\\*\\\] qualifier may not be used in declaration of pointers" } */
