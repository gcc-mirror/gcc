/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops" } */

unsigned int *a;

unsigned int __attribute__((noclone,noinline))
f (unsigned int n)
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
