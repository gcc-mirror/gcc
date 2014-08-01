/* UPC does not allow a pointer of type `shared void *'
   to be used in arithmetic.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

typedef __SIZE_TYPE__ size_t;
extern int MYTHREAD;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int A[THREADS];
shared void *pts;
shared int  *pts2;
size_t diff;

int main()
{
  pts = (shared void *)&A[0];
  pts += (MYTHREAD + 1) % THREADS; /* { dg-error "UPC does not allow a pointer of type 'shared void \\*' to be used in arithmetic" } */
  return 0;
}
