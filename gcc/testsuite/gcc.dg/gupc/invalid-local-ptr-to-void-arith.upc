/* The consensus of the UPC community seems to be that
   arithmetic on (void *) pointers is a compilation error.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int A[10];
void *p;

int main()
{
  p = (void *)A;
  p = p + 1; /* { dg-error "pointer of type 'void \\*' used in arithmetic \\\[-Werror=pointer-arith\\\]" } */
  *((int *)p) = 1;
  return 0;
}
/* { dg-excess-errors "some warnings being treated as errors" } */
