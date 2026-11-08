/* Unmasked call of an inbranch SIMD clone whose mask argument is a
   floating-point vector.  */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd --param vect-partial-vector-usage=0" } */
/* { dg-additional-options "-mavx512f" { target avx512f_runtime } } */

#include "tree-vect.h"

#define N 64

#pragma omp declare simd inbranch
__attribute__((noinline)) double
foo (double x, double y)
{
  return x * 10.0 + y;
}

double r[N], t[N];

__attribute__((noipa)) void
f (double *rp, double *tp, long n)
{
  for (long i = 0; i < n; i++)
    rp[i] = rp[i] * (foo (tp[0] + 1.0, tp[0] + 2.0)
		     + foo (tp[0] + 3.0, tp[0] + 4.0));
}

int
main ()
{
  check_vect ();
  for (int i = 0; i < N; i++)
    {
      r[i] = i;
      t[i] = 0.5;
    }
  f (r, t, N);
  /* foo (1.5, 2.5) + foo (3.5, 4.5) = 17.5 + 39.5.  */
  for (int i = 0; i < N; i++)
    if (r[i] != i * 57.0)
      __builtin_abort ();
  return 0;
}
