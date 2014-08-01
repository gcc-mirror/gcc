/* 	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


shared [] int *p0;

shared int *p1;

int
main ()
{
  return p0 > p1; /* { dg-error "UPC does not allow comparison between pointers to shared with differing block sizes without a cast" } */
}
