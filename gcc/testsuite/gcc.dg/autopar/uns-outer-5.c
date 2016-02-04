/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

void abort (void);

unsigned int x[500][500];
unsigned int y[500];
unsigned int g_sum=0;

void __attribute__((noinline))
init (int i, int j)
{
  x[i][j]=1;
}

void __attribute__((noinline))
parloop (int N)
{
  int i, j;
  unsigned int sum;

  /* Inner cycle is currently not supported, outer loop is not
     parallelized.  Inner reduction is detected, inner loop is
     parallelized.  */
  for (i = 0; i < N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++)
	sum += x[i][j];
      y[i]=sum;
    }
  g_sum = sum;
}

int
main (void)
{
  int i, j;
  for (i = 0; i < 500; i++)
    for (j = 0; j < 500; j++)
      init (i, j);

  parloop (500);

  return 0;
}

/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "loopfn" 4 "optimized" } } */
