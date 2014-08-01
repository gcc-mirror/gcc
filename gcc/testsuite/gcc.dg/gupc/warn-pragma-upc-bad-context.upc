/* Warning: #pragma upc not allowed in this context.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];
extern void __putsi2 (upc_shared_ptr_t, int);


shared int v;

shared int *pts;

int main (void)
{
  pts = &v;
  /* pragma upc must appear contextually as first token
     after opening brace.  Invalid here.  */
  #pragma upc strict /* { dg-warning "#pragma upc not allowed in this context \\\[-Wpragmas\\\]" } */
  *pts = 1;
  return 0;
}
