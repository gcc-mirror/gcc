/* UPC does not allow casts from a local pointer to a pointer-to-shared.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];
extern void __putsi2 (upc_shared_ptr_t, int);



int x;

int main (void)
{
  int *local_ptr = &x;
  shared int *pts = (shared int *)local_ptr; /* { dg-error "UPC does not allow casts from a local pointer to a pointer-to-shared" } */
  *pts = 1;
  return 0;
}
