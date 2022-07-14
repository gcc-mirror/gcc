/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

float d[N];
int e[N];
unsigned short f[N];

#pragma omp declare simd simdlen(8) notinbranch uniform(b)
__attribute__((noinline)) float
foo (float a, float b, float c)
{
  if (a < 30)
    return 5.0f;
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
      e[i] = e[i] * 3;
      f[i] = f[i] + 1;
    }
}

int
main ()
{
  int i;
  check_vect ();
  bar ();
  for (i = 0; i < N; i++)
    if (d[i] != (i < 30 ? 5.0f : i * 4 + 123.0f) || e[i] || f[i] != 1)
      abort ();
  return 0;
}

/* { dg-warning {unsupported simdlen 8 \(amdgcn\)} "" { target amdgcn*-*-* } 17 } */
