/* Warning: missing parameter after #pragma upc.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



#pragma upc  /* { dg-warning "missing parameter after #pragma upc \\\[-Wpragmas\\\]" } */
shared int x;

