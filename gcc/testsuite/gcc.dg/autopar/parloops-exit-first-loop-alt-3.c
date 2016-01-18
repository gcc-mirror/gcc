/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops2-details" } */

/* Variable bound, reduction.  */

unsigned int *a;

unsigned int
f (unsigned int n, unsigned int *__restrict__ a)
{
  int i;
  unsigned int sum = 1;

  for (i = 0; i < n; ++i)
    sum += a[i];

  return sum;
}

/* { dg-final { scan-tree-dump-times "alternative exit-first loop transform succeeded" 1 "parloops2" } } */
