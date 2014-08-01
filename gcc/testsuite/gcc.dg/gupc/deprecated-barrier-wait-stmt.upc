/* ‘barrier_wait’ was supported in version 1.0 of the UPC specification,
    it has been deprecated, use ‘upc_wait’ instead.
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



int main()
{
   barrier_wait; /* { dg-error "'barrier_wait' was supported in version 1.0 of the UPC specification, it has been deprecated, use 'upc_wait' instead" } */
   return 0;
}
