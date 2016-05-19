/* { dg-require-effective-target size32plus } */

#define N 24
#define M 1000

float A[1000][1000], B[1000][1000], C[1000][1000];

void test (void)
{
  int i, j, k;

  /* These loops contain too few iterations for being strip-mined by 64.  */
  for (i = 0; i < 24; i++)
    for (j = 0; j < 24; j++)
      for (k = 0; k < 24; k++)
        A[i][j] += B[i][k] * C[k][j];

  /* These loops should still be strip mined.  */
  for (i = 0; i < 1000; i++)
    for (j = 0; j < 1000; j++)
      for (k = 0; k < 1000; k++)
        A[i][j] += B[i][k] * C[k][j];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite"} } */
