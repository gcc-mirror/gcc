/* PR tree-optimization/46194 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-parallelize-loops=2 -fdump-tree-parloops2-details" } */

#define N 1000
int a[N];

int foo (void)
{
  int j;
  int i;

  /* This is not blocked as it is not profitable.  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[j] = a[i] + 1;

  return a[0];
}

/* This loop cannot be parallelized due to a dependence.  */

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 0 "parloops2" } } */
