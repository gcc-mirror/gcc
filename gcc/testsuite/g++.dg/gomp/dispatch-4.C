/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check that no host-to-device pointer conversion happens for a nullptr 
   argument. */

#include <cstddef>

int variant_fn(int *, int * = NULL);

#pragma omp declare variant(variant_fn) match(construct={dispatch}) adjust_args(need_device_ptr : x, y)
int bar(int *x, int *y = NULL);

void sub(int *a, int *b)
{
  int x;
  #pragma omp dispatch
   x = bar(a);
} 

/* { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-not "__builtin_omp_get_mapped_ptr \\(OB" "gimple" } } */
