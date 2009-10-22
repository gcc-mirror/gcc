/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-optimized" } */

void abort (void);

void parloop (int N)
{
  int i, j;
  int x[10000][10000];

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      x[i][j] = i + j + 3;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (x[i][j] != i + j + 3)
	abort ();
}

int main(void)
{
  parloop(10000);

  return 0;
}


/* Check that outer loop is parallelized.  */
/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 5 "optimized" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
