/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops" } */

/* Variable bound, vector addition, unsigned loop counter, signed bound.  */

void
f (int n, unsigned int *__restrict__ a, unsigned int *__restrict__ b,
   unsigned int *__restrict__ c)
{
  unsigned int i;

  for (i = 0; i < n; ++i)
    c[i] = a[i] + b[i];
}

/* Three times a store:
   - one in f._loopfn.0
   - one in the parallel
   - one in the low iteration count loop
   Crucially, none for a peeled off last iteration following the parallel.  */
/* { dg-final { scan-tree-dump-times "(?n)^  \\*_\[0-9\]*" 3 "parloops" } } */
