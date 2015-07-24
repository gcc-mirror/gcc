/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops-details" } */

/* Constant bound, reduction.  */

#define N 4000

unsigned int *a;

unsigned int
f (void)
{
  int i;
  unsigned int sum = 1;

  for (i = 0; i < N; ++i)
    sum += a[i];

  return sum;
}

/* { dg-final { scan-tree-dump-times "alternative exit-first loop transform succeeded" 1 "parloops" } } */
