/* { dg-require-effective-target size32plus } */
/* { dg-timeout-factor 4.0 } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 24
#define M 100

int A[M][M][M], B[M][M], C[M][M];

static int __attribute__((noinline))
foo (void)
{
  int i, j, k;

  /* These loops contain too few iterations to be blocked by 64.  */
  for (i = 0; i < 24; i++)
    for (j = 0; j < 24; j++)
      for (k = 0; k < 24; k++)
        A[i][j][k] = B[i][k] * C[k][j];

  /* These loops should still be loop blocked.  */
  for (i = 0; i < M; i++)
    for (j = 0; j < M; j++)
      for (k = 0; k < M; k++)
        A[i][j][k] = B[i][k] * C[k][j];

  return A[0][0][0] + A[M-1][M-1][M-1];
}

extern void abort ();

int
main (void)
{
  int i, j, res;

  for (i = 0; i < M; i++)
    for (j = 0; j < M; j++)
      {
	B[i][j] = i;
	C[i][j] = j;
      }

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 9801)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
