/* Test OpenACC 'kernels' construct decomposition.  */

/* { dg-additional-options "-fchecking --param=openacc-kernels=decompose" } */
/* { dg-ice "TODO" }
   { dg-prune-output "during GIMPLE pass: omplower" } */

/* Reduced from 'kernels-decompose-ice-1.c'.  */

int
main ()
{
#pragma acc kernels
  {
    int i;
  }
}
