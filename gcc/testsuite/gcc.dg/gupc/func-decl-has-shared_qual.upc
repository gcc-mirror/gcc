/* Function definition has UPC shared qualified return type. 
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int return_shared_int (void) /* { dg-error "function definition has UPC shared qualified return type" } */
{
  return 0;
}
