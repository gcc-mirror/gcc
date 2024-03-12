/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int d[N], e[N];

#pragma omp declare simd simdlen(4) notinbranch uniform(b) linear(c:3)
__attribute__((noinline)) int
foo (int a, int b, int c)
{
  if (a < 30)
    return 5;
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
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (d[i] != (i < 30 ? 5 : i * 4 + 123) || e[i] != i)
      abort ();
  return 0;
}

/* { dg-warning {unsupported simdlen 4 \(amdgcn\)} "" { target amdgcn*-*-* } 15 } */
