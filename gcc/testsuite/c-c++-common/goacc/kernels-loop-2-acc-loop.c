/* { dg-additional-options "--param=openacc-kernels=parloops" } as this is
   specifically testing "parloops" handling.  */
/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-parloops1-all" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

/* Check that loops with '#pragma acc loop' tagged gets properly parallelized.  */
#define ACC_LOOP
#include "kernels-loop-2.c"

/* Check that only three loops are analyzed, and that all can be
   parallelized.  */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 3 "parloops1" } } */
/* { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint, noclone\\)\\)" 3 "parloops1" } } */
/* { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } } */

/* Check that the loop has been split off into a function.  */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*main._omp_fn.0" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*main._omp_fn.1" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n);; Function .*main._omp_fn.2" 1 "optimized" } } */
