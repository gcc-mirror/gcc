/* Parameter declared with UPC shared qualifier. 
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int decl_shared_parameter (shared int p) /* { dg-error "parameter declared with UPC shared qualifier" } */
{
  return p;
}
