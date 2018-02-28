/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define N (1024)
double a[N][N], b[N][N], c[N][N];

void
foo (void)
{
  unsigned i, j, k;

  for (i = 0; i < N; ++i)
    for (j = 0; j < N; ++j)
      {
	c[i][j] = 0.0;
	for (k = 0; k < N; ++k)
	  c[i][j] += a[i][k] * b[k][j];
      }
}

/* { dg-final { scan-tree-dump "Loop nest . distributed: split to 1 loops and 1 library" "ldist" } } */
