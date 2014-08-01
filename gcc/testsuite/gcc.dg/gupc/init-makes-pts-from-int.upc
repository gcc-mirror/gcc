/* Initialization attempts to make a UPC pointer-to-shared value
   from an integer without a cast.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared int *p_4000;

int main()
{
   shared int *p = 0x4000; /* { dg-error "initialization attempts to make a UPC pointer-to-shared value from an integer without a cast" } */
   p_4000 = p;
   return 0;
}
