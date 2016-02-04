/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

void abort (void);

unsigned int g_sum=0;
unsigned int x[500][500];

void
parloop (int N)
{
  int i, j;
  unsigned int sum;

  /* Double reduction is detected, outer loop is parallelized.  */
  sum = 0;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum += x[i][j];

  g_sum = sum;
}

/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 4 "optimized" } } */
