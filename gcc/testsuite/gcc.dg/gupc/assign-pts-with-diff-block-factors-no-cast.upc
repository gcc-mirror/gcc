/* UPC does not allow assignment
   between pointers to shared with
   differing block sizes without a cast.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [3] int A3[3*THREADS];
shared [5] int A5[5*THREADS];

shared [3] int *p3 = A3;
shared [5] int *p5 = A5;

int main (void)
{
  p3 = p5; /* { dg-error "UPC does not allow assignment between pointers to shared with differing block sizes without a cast" } */
  return 0;
}
