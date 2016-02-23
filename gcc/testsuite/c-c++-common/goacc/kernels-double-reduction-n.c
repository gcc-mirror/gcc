/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-parloops1-all" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#include <stdlib.h>

#define N 500

unsigned int a[N][N];

void  __attribute__((noinline,noclone))
foo (unsigned int n)
{
  int i, j;
  unsigned int sum = 1;

#pragma acc kernels copyin (a[0:n]) copy (sum)
  {
    for (i = 0; i < n; ++i)
      for (j = 0; j < n; ++j)
	sum += a[i][j];
  }

  if (sum != 5001)
    abort ();
}

/* Check that only one loop is analyzed, and that it can be parallelized.  */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } } */
/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops1" } } */

/* Check that the loop has been split off into a function.  */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*foo.*._omp_fn.0" 1 "optimized" } } */

/* { dg-final { scan-tree-dump-times "(?n)oacc function \\(0," 1 "parloops1" } } */
