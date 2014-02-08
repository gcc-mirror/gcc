/* PR c/59984 */
/* { dg-additional-options "-fopenmp-simd" } */

#include "tree-vect.h"

#define N 128

int a[N];

#pragma omp declare simd
__attribute__((noinline)) void
foo (int in, int *out1, int *out2)
{
  *out1 = in * in - 1;
  *out2 = in * in + 1;
}

#pragma omp declare simd linear (out1, out2)
__attribute__((noinline)) void
bar (int in, int *out1, int *out2)
{
  *out1 = in * in - 1;
  *out2 = in * in + 1;
}

__attribute__((noinline)) void
test (void)
{
  int i;
  for (i = 0; i < N; i++)
    a[i] = i;
#pragma omp simd
  for (i = 0; i < N; i++)
    {
      int v1, v2;
      foo (a[i], &v1, &v2);
      a[i] = v1 * v2;
    }
  for (i = 0; i < N; i++)
    if (a[i] != i * i * i * i - 1)
      __builtin_abort ();
  for (i = 0; i < N; i++)
    a[i] = i;
#pragma omp simd
  for (i = 0; i < N; i++)
    {
      int v1, v2;
      bar (a[i], &v1, &v2);
      a[i] = v1 * v2;
    }
  for (i = 0; i < N; i++)
    if (a[i] != i * i * i * i - 1)
      __builtin_abort ();
}

int
main ()
{
  check_vect ();
  test ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
