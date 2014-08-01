/* UPC operator applied to a non-shared type.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

typedef __SIZE_TYPE__ size_t;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int x[10];
size_t s;

int main()
{
  s = upc_localsizeof (x); /* { dg-error "UPC operator upc_localsizeof applied to a non-shared type" } */
  return 0;
}
