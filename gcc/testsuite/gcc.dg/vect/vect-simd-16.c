/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 3 "vect" { target i?86-*-* x86_64-*-* } } } */

#include "tree-vect.h"

__attribute__((noipa)) int
foo (int *a)
{
  int i;
  #pragma omp simd lastprivate (i)
  for (i = 0; i < 64; i++)
    a[i] = i;
  return i;
}

__attribute__((noipa)) void
bar (int *a)
{
  int i;
  #pragma omp simd private (i)
  for (i = 0; i < 64; i++)
    a[i] = i + 1;
}

__attribute__((noipa)) int
baz (int *a)
{
  int i;
  #pragma omp simd linear (i)
  for (i = 0; i < 64; i++)
    a[i] = i + 2;
  return i;
}

int
main ()
{
  int i;
  int a[64];
  check_vect ();
  if (foo (a) != 64)
    abort ();
#pragma GCC novector
  for (i = 0; i < 64; ++i)
    if (a[i] != i)
      abort ();
    else
      a[i] = -8;
  bar (a);
#pragma GCC novector
  for (i = 0; i < 64; ++i)
    if (a[i] != i + 1)
      abort ();
    else
      a[i] = -8;
  if (baz (a) != 64)
    abort ();
#pragma GCC novector
  for (i = 0; i < 64; ++i)
    if (a[i] != i + 2)
      abort ();
  return 0;
}
