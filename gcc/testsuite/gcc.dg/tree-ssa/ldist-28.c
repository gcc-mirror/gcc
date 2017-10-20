/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define M (256)
#define N (1024)
int arr[M][N];

void
foo (void)
{
  for (unsigned i = 0; i < M; ++i)
    for (unsigned j = 0; j < N; ++j)
      arr[i][j] = 0;
}

/* { dg-final { scan-tree-dump "Loop nest . distributed: split to 0 loops and 1 library" "ldist" } } */
