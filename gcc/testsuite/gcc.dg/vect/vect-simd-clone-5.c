/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int d[N], e[N];

#pragma omp declare simd simdlen(4) notinbranch uniform(b) linear(c:3)
__attribute__((noinline)) long long int
foo (int a, int b, int c)
{
  return a + b + c;
}

__attribute__((noinline, noclone)) void
bar ()
{
  int i;
#pragma omp simd
  for (i = 0; i < N; ++i)
    {
      d[i] = foo (i, 123, i * 3);
      e[i] = e[i] + i;
    }
}

int
main ()
{
  int i;
  check_vect ();
  bar ();
  for (i = 0; i < N; i++)
    if (d[i] != i * 4 + 123 || e[i] != i)
      abort ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
