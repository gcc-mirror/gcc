/* UPC forbids the declaration of a variable-size shared array. 
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



void declare_shared_vla (int N)
{
  shared int A[N*THREADS]; /* { dg-error "UPC does not support shared auto variables" } */
  A[0] = 1;
}
/* { dg-prune-output "UPC forbids variable-size shared array 'A'" } */
