/* UPC does not allow assignments
   from a pointer-to-shared to a local pointer.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int *pts;

int main (void)
{
  int *local_ptr = pts; /* { dg-error "UPC does not allow assignments from a pointer-to-shared to a local pointer" } */
  *local_ptr = 1;
  return 0;
}
