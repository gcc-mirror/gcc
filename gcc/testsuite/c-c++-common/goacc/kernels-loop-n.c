/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-parloops1-all" } */
/* { dg-additional-options "-fdump-tree-optimized" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

#define N ((1024 * 512) + 1)
#define COUNTERTYPE unsigned int

int
foo (COUNTERTYPE n)
{
  unsigned int *__restrict a;
  unsigned int *__restrict b;
  unsigned int *__restrict c;

  a = (unsigned int *__restrict)malloc (n * sizeof (unsigned int));
  b = (unsigned int *__restrict)malloc (n * sizeof (unsigned int));
  c = (unsigned int *__restrict)malloc (n * sizeof (unsigned int));

  for (COUNTERTYPE i = 0; i < n; i++)
    a[i] = i * 2;

  for (COUNTERTYPE i = 0; i < n; i++)
    b[i] = i * 4;

#pragma acc kernels copyin (a[0:n], b[0:n]) copyout (c[0:n])
  {
#ifdef ACC_LOOP
    #pragma acc loop
#endif
    for (COUNTERTYPE ii = 0; ii < n; ii++)
      c[ii] = a[ii] + b[ii];
  }

  for (COUNTERTYPE i = 0; i < n; i++)
    if (c[i] != a[i] + b[i])
      abort ();

  free (a);
  free (b);
  free (c);

  return 0;
}

/* Check that only one loop is analyzed, and that it can be parallelized.  */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint, noclone\\)\\)" 1 "parloops1" } } */
/* { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } } */

/* Check that the loop has been split off into a function.  */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*foo.*._omp_fn.0" 1 "optimized" } } */
