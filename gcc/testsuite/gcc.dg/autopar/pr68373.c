/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops2-details" } */

unsigned int
foo (int *a, unsigned int n)
{
  unsigned int i;
  for (i = 0; i < n; ++i)
    a[i] = 1;

  return i;
}

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops2" } } */
