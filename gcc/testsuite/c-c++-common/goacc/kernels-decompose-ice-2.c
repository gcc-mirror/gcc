/* Test OpenACC 'kernels' construct decomposition.  */

/* { dg-additional-options "-fopt-info-omp-all" } */

/* { dg-additional-options "-fchecking --param=openacc-kernels=decompose" } */
/* { dg-ice "TODO" }
   { dg-prune-output "during GIMPLE pass: omplower" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

/* Reduced from 'kernels-decompose-ice-1.c'.  */

int
main ()
{
#pragma acc kernels
  /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .-1 } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } .-2 } */
  {
    int i;
  }
}
