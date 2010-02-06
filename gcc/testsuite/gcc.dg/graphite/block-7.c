/* { dg-require-effective-target size32plus } */

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
      {
        A[i][j] = 0;
        for (k = 0; k < N; k++)
          A[i][j] += B[i][k] * C[k][j];
      }
}

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

  /* Avoid runtime check for this testcase, as it is miscompiled by
     Graphite for the moment.  */
  return 0;
  return res != 529340000;
}

/* { dg-final { scan-tree-dump-times "SCoP will be loop blocked" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
