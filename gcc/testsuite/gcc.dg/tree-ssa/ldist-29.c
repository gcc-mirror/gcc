/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define M (256)
#define N (512)
int arr[M][N];

void
foo (void)
{
  for (unsigned i = 0; i < M; ++i)
    for (unsigned j = 0; j < N - 1; ++j)
      arr[i][j] = 0;
}

/* { dg-final { scan-tree-dump-not "Loop nest . distributed: split to" "ldist" } } */
/* { dg-final { scan-tree-dump-times "Loop . distributed: split to 0 loops and 1 library" 1 "ldist" } } */
