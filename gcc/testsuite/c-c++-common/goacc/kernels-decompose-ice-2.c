/* Test OpenACC 'kernels' construct decomposition.  */

/* { dg-additional-options "-fopenacc-kernels=decompose" } */
/* { dg-ice "TODO" }
   { dg-prune-output "during GIMPLE pass: omp_oacc_kernels_decompose" } */

/* Reduced from 'kernels-decompose-ice-1.c'.  */

int
main ()
{
#pragma acc kernels
  {
    int i;
  }
}
