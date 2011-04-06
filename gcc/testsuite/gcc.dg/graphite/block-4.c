/* { dg-require-effective-target size32plus } */
/* { dg-require-effective-target run_expensive_tests }  PR testsuite/48283 */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 24
#define M 1000

int A[M][M], B[M][M], C[M][M];

static int __attribute__((noinline))
foo (void)
{
  int i, j, k;

  /* This should NOT be blocked: each loop iterates only 24 times.  */
  for (i = 0; i < 24; i++)
    for (j = 0; j < 24; j++)
      for (k = 0; k < 24; k++)
        A[i][j] = B[i][k] * C[k][j];

  /* This should be blocked.  */
  for (i = 0; i < M; i++)
    for (j = 0; j < M; j++)
      for (k = 0; k < M; k++)
        A[i][j] = B[i][k] * C[k][j];

  return A[0][0] + A[M-1][M-1];
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

  if (res != 998001)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 1 "graphite" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
