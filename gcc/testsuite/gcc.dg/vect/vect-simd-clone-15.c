/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int array[N];

#pragma omp declare simd linear(val(b):-3), notinbranch
__attribute__((noinline)) int
foo (int a, int b)
{
  return a + b;
}

__attribute__((noinline, noclone)) void
bar ()
{
  int i;
#pragma omp simd
  for (i = 0; i < N; ++i)
    array[i] = foo (i >> 1, -i * 3);
}

int
main ()
{
  int i;
  check_vect ();
  bar ();
  for (i = 0; i < N; i++)
    if (array[i] != ((i >> 1) + (-3 * i)))
      abort ();
  return 0;
}
