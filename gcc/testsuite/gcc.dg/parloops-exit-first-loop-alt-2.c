/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops" } */

#define N 1000

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];

void __attribute__((noclone,noinline))
f (unsigned int n)
{
  int i;

    for (i = 0; i < N; ++i)
      c[i] = a[i] + b[i];
}

/* Three times three array accesses:
   - three in f._loopfn.0
   - three in the parallel
   - three in the low iteration count loop
   Crucially, none for a peeled off last iteration following the parallel.  */
/* { dg-final { scan-tree-dump-times "(?n)\\\[i" 9 "parloops" } } */
