/* UPC layout qualifier of the form [*] cannot be
   applied to an incomplete type.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [*] struct s_struct A5_icomplete[5*THREADS]; /* { dg-error "array type has incomplete element type" } */
