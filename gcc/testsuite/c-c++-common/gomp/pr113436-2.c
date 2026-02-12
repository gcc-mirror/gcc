/* PR middle-end/113436 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

#include <omp.h>

void
g()
{
  int A, B[10], *C;
  A = 5;
  C = (int *) __builtin_malloc (sizeof (int) * 10);
  for (int i = 0; i < 10; i++)
    B[i] = C[i] = i+5;
      
  #pragma omp target firstprivate(A) firstprivate(B) firstprivate(C) allocate(allocator(omp_high_bw_mem_alloc), align(64): A, B, C)
    {
      A = 99;
      for (int i = 0; i < 10; i++)
        B[i] = -i-23;
      C = &A;
    }
}

/* { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(64, 4, 4\\\);" "omplower" { target int32 } } } */
/* { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(64, 40, 4\\\);" "omplower" { target int32 } } } */
/* { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(64, 8, 4\\\);" "omplower" { target { lp64 || llp64 } } } } */
/* { dg-final { scan-tree-dump-times "\\\*D\\\.\[0-9\]\+ = D\\\.\[0-9\]\+;" 3 "omplower" } } */
/* { dg-final { scan-tree-dump "\\\(\\\*D\\\.\[0-9\]\+\\\) = \\\(\\\*D\\\.\[0-9\]\+\\\);" "omplower" } } */
/* { dg-final { scan-tree-dump "\\\*D\\\.\[0-9\]\+ = 99;" "omplower" } } */
/* { dg-final { scan-tree-dump "\\\(\\\*D\\\.\[0-9\]\+\\\)\\\[i\\\] = D\\\.\[0-9\]\+;" "omplower" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\\(D\\\.\[0-9\]+, 4\\\)" 3 "omplower" } } */
