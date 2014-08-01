/* Passing argument attempts to make a UPC pointer-to-shared
   value from an integer.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];




extern void proc (shared int *arg);

int main()
{
   proc (0x4000); /* { dg-error "passing argument 1 of 'proc' attempts to make a UPC pointer-to-shared value from an integer" } */
   return 0;
}
