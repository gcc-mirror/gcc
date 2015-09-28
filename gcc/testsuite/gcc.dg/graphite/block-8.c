/* { dg-require-effective-target size32plus } */
/* { dg-require-effective-target run_expensive_tests }  PR testsuite/48283 */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 200

int A[N][N], B[N][N], C[N][N];

static void __attribute__((noinline))
matmult (void)
{
  int i, j, k;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      A[i][j] = 0;

  /* This should be blocked.  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      for (k = 0; k < N; k++)
	A[i][j] += B[i][k] * C[k][j];
}

extern void abort ();

int
main (void)
{
  int i, j, res = 0;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      {
	B[i][j] = j;
	C[i][j] = i;
      }

  matmult ();

  for (i = 0; i < N; i++)
    res += A[i][i];

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 529340000)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "tiled by" "graphite" } } */
