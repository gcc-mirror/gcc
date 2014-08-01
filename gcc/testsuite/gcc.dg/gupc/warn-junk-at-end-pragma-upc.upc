/* Warning: junk at end of #pragma upc.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



#pragma upc strict junk /* { dg-warning "junk at end of #pragma upc \\\[-Wpragmas\\\]" } */
shared int x;

