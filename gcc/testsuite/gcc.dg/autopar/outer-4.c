/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

void abort (void);

int g_sum=0;
int x[500][500];

void
parloop (int N)
{
  int i, j;
  int sum;

  /* The inner reduction is not recognized as reduction because we cannot assume
     that int wraps on overflow.  The way to fix this is to implement the
     reduction operation in unsigned type, but we've not yet implemented
     this.  */
  sum = 0;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum += x[i][j];

  g_sum = sum;
}


/* { dg-final { scan-tree-dump-times "parallelizing inner loop" 0 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "loopfn" 4 "optimized" { xfail *-*-* } } } */
