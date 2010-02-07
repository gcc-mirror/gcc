/* { dg-require-effective-target size32plus } */

/* Formerly known as ltrans-6.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 100
#define M 200

static int __attribute__((noinline))
foo (int A[N][M])
{
  int i, j;

  /* This loop should be interchanged. */
  for(j = 0; j < M; j++)
    for(i = 0; i < N; i++)
      A[i][j] = A[i][j] + A[i][j];

  return A[0][0] + A[N-1][M-1];
}

extern void abort ();

int
main (void)
{
  int A[N][M];
  int i, j, res;

  for (i = 0; i < N; i++)
    for (j = 0; j < M; j++)
      A[i][j] = 2;

  res = foo (A);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 8)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
