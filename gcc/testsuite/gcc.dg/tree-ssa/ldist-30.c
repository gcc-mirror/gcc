/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define M (256)
#define N (512)
int a[M][N], b[M][N];

void
foo (void)
{
  for (unsigned i = 0; i < M; ++i)
    for (unsigned j = N; j > 0; --j)
      a[i][j - 1] = b[i][j - 1];
}

/* { dg-final { scan-tree-dump-times "Loop nest . distributed: split to" 1 "ldist" } } */
