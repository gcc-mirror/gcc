/* { dg-require-effective-target size32plus } */

/* Formerly known as ltrans-5.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 100
#define M 1111
int A[N][M];

static int __attribute__((noinline))
foo (void)
{
  int i, j;

  for( i = 0; i < M; i++)
    for( j = 0; j < N; j++)
      A[j][i] = 5 * A[j][i];

  return A[0][0] + A[N-1][M-1];
}

extern void abort ();

int
main (void)
{
  int i, j, res;

  for (i = 0; i < N; i++)
    for (j = 0; j < M; j++)
      A[i][j] = 2;

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 20)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
