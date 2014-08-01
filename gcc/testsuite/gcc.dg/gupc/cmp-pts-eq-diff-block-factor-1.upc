/* 	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


shared [10] int *p10;

shared [20] int *p20;

int
main ()
{
  return p10 == p20; /* { dg-error "UPC does not allow comparison between pointers to shared with differing block sizes without a cast" } */
}
