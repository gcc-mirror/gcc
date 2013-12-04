/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int a[N] __attribute__((aligned (32)));

#pragma omp declare simd linear(a) linear(b:3) linear(c:6) notinbranch
__attribute__((noinline)) int
foo (int a, int b, int c)
{
  return a ^ (b * 512) ^ (c * 512 * 512);
}

__attribute__((noinline, noclone)) void
bar (int *d)
{
  int i, j, k;
  for (i = 0, j = 0, k = 0; i < N / 2; i++, j++, k += 3)
    d[i] = foo (j, i * 3, 2 * k + 2);
}

#if 0
__attribute__((noinline, noclone)) void
baz (int *d)
{
  long int i, j, k;
  for (i = 0, j = 0, k = 0; i < N / 2;
       i = (int) i + 1, j = (int) j + 1, k = (int) k + 3)
    d[i] = foo (j, i * 3, 2 * k + 2);
}
#endif

int
main ()
{
  int i;
  check_vect ();
  if (sizeof (int) * __CHAR_BIT__ < 32)
    return 0;
  bar (a + 7);
  for (i = 0; i < N / 2; i++)
    if (a[i + 7] != (i ^ (i * 3 * 512) ^ (((i * 6) + 2) * 512 * 512)))
      abort ();
  bar (a);
  for (i = 0; i < N / 2; i++)
    if (a[i] != (i ^ (i * 3 * 512) ^ (((i * 6) + 2) * 512 * 512)))
      abort ();
#if 0
  baz (a + 7);
  for (i = 0; i < N / 2; i++)
    if (a[i + 7] != (i ^ (i * 3 * 512) ^ (((i * 6) + 2) * 512 * 512)))
      abort ();
  baz (a);
  for (i = 0; i < N / 2; i++)
    if (a[i] != (i ^ (i * 3 * 512) ^ (((i * 6) + 2) * 512 * 512)))
      abort ();
#endif
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
