/* Invalid & operation applied to a UPC shared bit field.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


shared struct 
  {
    int a;
    int b : 8;
    int c : 24;
  } x;

shared int *pts;

int main (void)
{
  pts = &x.b; /* { dg-error "cannot take address of bit-field 'b'" } */
  return 0;
}
