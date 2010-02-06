/* { dg-require-effective-target size32plus } */

/* Formerly known as ltrans-8.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 111
#define M 1111

static int __attribute__((noinline))
foo (double *a)
{
  int i,j;
  int r = 0;

  for (i = 0; i < N; ++i)
    for (j = 0; j < M; ++j)
      r += a[j * N + i];

  return r;
}

int
main (void)
{
  double A[N*M];
  int i, res;

  for (i = 0; i < N*M; i++)
    A[i] = 2;

  res = foo (A);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  return res != 246642;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
