/* Invalid application of <op> to shared void.
   (where <op> is sizeof(0) or __alignof())
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

typedef __SIZE_TYPE__ size_t;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int A[THREADS];

size_t size;

int main()
{
   shared void *p = (shared void *)A;
   size = sizeof (*p); /* { dg-error "invalid application of 'sizeof' to 'shared void' type" } */
   return 0;
}
