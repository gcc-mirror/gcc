/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops2-details" } */

/* Variable bound, vector addition, unsigned loop counter, signed bound.  */

void
f (int n, unsigned int *__restrict__ a, unsigned int *__restrict__ b,
   unsigned int *__restrict__ c)
{
  unsigned int i;

  for (i = 0; i < n; ++i)
    c[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump-times "alternative exit-first loop transform succeeded" 1 "parloops2" } } */
