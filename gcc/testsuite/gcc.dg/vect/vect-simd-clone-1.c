/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int array[N];

#pragma omp declare simd simdlen(4) notinbranch
#pragma omp declare simd simdlen(4) notinbranch uniform(b) linear(c:3)
#pragma omp declare simd simdlen(8) notinbranch
#pragma omp declare simd simdlen(8) notinbranch uniform(b) linear(c:3)
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
    array[i] = foo (i, 123, i * 3);
}

__attribute__((noinline, noclone)) void
baz ()
{
  int i;
#pragma omp simd
  for (i = 0; i < N; ++i)
    array[i] = foo (i, array[i], i * 3);
}

int
main ()
{
  int i;
  check_vect ();
  bar ();
  for (i = 0; i < N; i++)
    if (array[i] != (i < 30 ? 5 : i * 4 + 123))
      abort ();
  baz ();
  for (i = 0; i < N; i++)
    if (array[i] != (i < 30 ? 5 : i * 8 + 123))
      abort ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
