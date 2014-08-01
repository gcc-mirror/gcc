/* UPC does not allow assignments from a local pointer
   to a pointer-to-shared.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int x;

shared int *pts;

int main (void)
{
  int *local_ptr = &x;
  pts = local_ptr; /* { dg-error "UPC does not allow assignments from a local pointer to a pointer-to-shared" } */
  return 0;
}
