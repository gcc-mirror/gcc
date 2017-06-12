/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-parloops1-all" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#include <stdlib.h>

#define N (1024 * 512)
#define COUNTERTYPE unsigned int

int
main (void)
{
  unsigned int i;

  unsigned int *__restrict c;

  c = (unsigned int *__restrict)malloc (N * sizeof (unsigned int));

  for (COUNTERTYPE i = 0; i < N; i++)
    c[i] = i * 2;

#pragma acc kernels copy (c[0:N])
  {
#ifdef ACC_LOOP
    #pragma acc loop
#endif
    for (COUNTERTYPE ii = 0; ii < N; ii++)
      c[ii] = c[ii] + ii + 1;
  }

  for (COUNTERTYPE i = 0; i < N; i++)
    if (c[i] != i * 2 + i + 1)
      abort ();

  free (c);

  return 0;
}

/* Check that only one loop is analyzed, and that it can be parallelized.  */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint\\)\\)" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } } */

/* Check that the loop has been split off into a function.  */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*main._omp_fn.0" 1 "optimized" } } */
