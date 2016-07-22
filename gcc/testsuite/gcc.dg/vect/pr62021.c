/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#pragma omp declare simd linear(y)
__attribute__((noinline)) int *
foo (int *x, int y)
{
  return x + y;
}

int a[1024];
int *b[1024] = { &a[0] };

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 1024; i++)
    b[i] = &a[1023 - i];
  #pragma omp simd
  for (i = 0; i < 1024; i++)
    b[i] = foo (b[i], i);
  for (i = 0; i < 1024; i++)
    if (b[i] != &a[1023])
      __builtin_abort ();
  return 0;
}

