#define N 1000

float A[N][N], B[N][N], C[N][N];

void matmult ()
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

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
