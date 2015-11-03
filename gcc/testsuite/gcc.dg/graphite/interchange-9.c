/* { dg-require-effective-target size32plus } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 111
#define M 111

static int __attribute__((noinline))
foo (int *x)
{
  int i, j;
  int sum = 0;

  for (j = 0; j < M; ++j)
    for (i = 0;  i < N; ++i)
      sum += x[M * i + j];

  return sum;
}

extern void abort ();

int
main (void)
{
  int A[N*M];
  int i, res;

  for (i = 0; i < N*M; i++)
    A[i] = 2;

  res = foo (A);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 24642)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "tiled" "graphite" } } */
