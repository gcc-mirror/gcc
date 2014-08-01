/* UPC operator applied to a function type.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

typedef __SIZE_TYPE__ size_t;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];




void func(void) {}

size_t s;

int main()
{
  s = upc_elemsizeof (func); /* { dg-error "UPC operator upc_elemsizeof applied to a function type" } */
  return 0;
}
