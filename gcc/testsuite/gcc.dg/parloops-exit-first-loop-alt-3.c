/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops" } */

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

/* Three array accesses:
   - one in f._loopfn.0
   - one in the parallel
   - one in the low iteration count loop
   Crucially, none for a peeled off last iteration following the parallel.  */
/* { dg-final { scan-tree-dump-times "(?n)\\\* 4" 3 "parloops" } } */
