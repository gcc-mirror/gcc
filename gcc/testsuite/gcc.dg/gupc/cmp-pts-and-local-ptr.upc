/* UPC does not allow comparisons between pointers to shared and
   local pointers.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int x;
shared int v;

shared int *pts;
int *local_ptr;

int main (void)
{
  local_ptr = &x;
  pts = &v;
  return pts == local_ptr; /* { dg-error "UPC does not allow comparisons between pointers to shared and local pointers" } */
}
