/* UPC does not support shared auto variables.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int decl_shared_local (int p)
{
  shared int x = p; /* { dg-error "UPC does not support shared auto variables" } */
  return x;
}
