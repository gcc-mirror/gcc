/* UPC operator applied to a void type.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

typedef __SIZE_TYPE__ size_t;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



void *p;
size_t s;


int main()
{
  s = upc_blocksizeof (*p); /* { dg-error "invalid use of void expression" } */
  return 0;
}
