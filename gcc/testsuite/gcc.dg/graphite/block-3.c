/* { dg-require-effective-target size32plus } */

#define N 24
#define M 100

float A[M][M][M], B[M][M], C[M][M];

void test (void)
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
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
