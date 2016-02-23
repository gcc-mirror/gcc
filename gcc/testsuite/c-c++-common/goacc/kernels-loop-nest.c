/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-parloops1-all" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

/* Based on autopar/outer-1.c.  */

#include <stdlib.h>

#define N 1000

int
main (void)
{
  int x[N][N];

#pragma acc kernels copyout (x)
  {
    for (int ii = 0; ii < N; ii++)
      for (int jj = 0; jj < N; jj++)
	x[ii][jj] = ii + jj + 3;
  }

  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      if (x[i][j] != i + j + 3)
	abort ();

  return 0;
}

/* Check that only one loop is analyzed, and that it can be parallelized.  */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } } */

/* Check that the loop has been split off into a function.  */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*main._omp_fn.0" 1 "optimized" } } */

/* { dg-final { scan-tree-dump-times "(?n)oacc function \\(0," 1 "parloops1" } } */
