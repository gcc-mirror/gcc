/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-require-effective-target vect_double } */

void
gemm (const double* __restrict__ A, const double* __restrict__ B,
      double* __restrict__ C)
{
  unsigned int l_m = 0;
  unsigned int l_n = 0;
  unsigned int l_k = 0;

  for ( l_n = 0; l_n < 9; l_n++ ) {
    /* Use -O3 so this loop is unrolled completely early.  */
    for ( l_m = 0; l_m < 10; l_m++ ) { C[(l_n*10)+l_m] = 0.0; }
    for ( l_k = 0; l_k < 17; l_k++ ) {
      /* Use -O3 so this loop is unrolled completely early.  */
      for ( l_m = 0; l_m < 10; l_m++ ) {
        C[(l_n*10)+l_m] += A[(l_k*20)+l_m] * B[(l_n*20)+l_k];
      }
    }
  }
}

/* Exact scanning is difficult but we expect all loads and stores
   and computations to be vectorized.  */
/* { dg-final { scan-tree-dump "optimized: basic block" "slp1" } } */
