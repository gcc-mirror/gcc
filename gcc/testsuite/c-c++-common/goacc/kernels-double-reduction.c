/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fopt-info-optimized-omp" } */
/* { dg-additional-options "-fdump-tree-parloops1-all" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#include <stdlib.h>

#define N 500

unsigned int a[N][N];

void  __attribute__((noinline,noclone))
foo (void)
{
  int i, j;
  unsigned int sum = 1;

#pragma acc kernels copyin (a[0:N]) copy (sum) /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
  {
    for (i = 0; i < N; ++i)
      for (j = 0; j < N; ++j)
	sum += a[i][j];
  }

  if (sum != 5001)
    abort ();
}

/* Check that only one loop is analyzed, and that it can be parallelized.  */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint, noclone, noinline\\)\\)" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } } */
/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops1" } } */

/* Check that the loop has been split off into a function.  */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*foo.*._omp_fn.0" 1 "optimized" } } */
