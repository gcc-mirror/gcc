/* The consensus of the UPC community seems to be that
   sizeof (void) is a compilation errors.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int main()
{
  return sizeof (void); /* { dg-error "invalid application of 'sizeof' to a void type \\\[-Werror=pointer-arith\\\]" } */
}

/* { dg-excess-errors "some warnings being treated as errors" } */
