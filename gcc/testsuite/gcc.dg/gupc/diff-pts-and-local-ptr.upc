/* Attempt to take the difference of a UPC pointer-to-shared
   and a local pointer.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int x;
shared int v;

shared int *pts;
int *local_ptr;
int diff;

int main (void)
{
  local_ptr = &x;
  pts = &v;
  diff = (pts - local_ptr); /* { dg-error "attempt to take the difference of a UPC pointer-to-shared and a local pointer" } */
  return 0;
}
