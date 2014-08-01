/* Return makes a UPC pointer-to-shared value from an integer.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int *func ()
{
  return 0x4000; /* { dg-error "return makes a UPC pointer-to-shared value from an integer" } */
}
