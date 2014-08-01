/* UPC layout qualifier cannot be applied to a void type.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [5] void *pts; /* { dg-error "UPC layout qualifier cannot be applied to a void type" } */
