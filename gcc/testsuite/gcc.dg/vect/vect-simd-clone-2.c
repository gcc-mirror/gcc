/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int array[N] __attribute__((aligned (32)));

#pragma omp declare simd simdlen(4) notinbranch aligned(a:16) uniform(a) linear(b)
#pragma omp declare simd simdlen(4) notinbranch aligned(a:32) uniform(a) linear(b)
#pragma omp declare simd simdlen(8) notinbranch aligned(a:16) uniform(a) linear(b)
#pragma omp declare simd simdlen(8) notinbranch aligned(a:32) uniform(a) linear(b)
__attribute__((noinline)) void
foo (int *a, int b, int c)
{
  a[b] = c;
}

__attribute__((noinline, noclone)) void
bar ()
{
  int i;
#pragma omp simd
  for (i = 0; i < N; ++i)
    foo (array, i, i * array[i]);
}

__attribute__((noinline, noclone)) void
baz ()
{
  int i;
  for (i = 0; i < N; i++)
    array[i] = 5 * (i & 7);
}

int
main ()
{
  int i;
  check_vect ();
  baz ();
  bar ();
  for (i = 0; i < N; i++)
    if (array[i] != 5 * (i & 7) * i)
      abort ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
