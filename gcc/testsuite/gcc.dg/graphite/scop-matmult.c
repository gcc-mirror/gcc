float A[1000][1000], B[1000][1000], C[1000][1000];

/* Multiply two n x n matrices A and B and store the result in C.  */

void matmult (int n)
{
  int i,j,k;

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      for (k = 0; k < n; k++)
        A[i][j] += B[i][k] * C[k][j];
}

/* This one fails because the number of iterations cannot be
   determined anymore for the outermost loop.  */
/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
