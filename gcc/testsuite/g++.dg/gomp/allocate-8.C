/* { dg-additional-options "-fdump-tree-gimple" } */
#include "allocate-allocator-handle.h"

/* Check simple cases that do not have an align clause and the allocator clause
   folds to omp_default_mem_alloc.  These cases are supposed to elide
   the usual transformations (calling __builtin_GOMP_alloc/__builtin_GOMP_free)
   that are usually applied to vars in an allocate directive.

   It is not entirely clear if this is correct behavior, and due to differences
   in how C++ works we can't support this for all cases, such as for exprs with
   calls to constexpr functions.
   At the very least we should make sure the obvious cases still work as
   expected.  */

/* { dg-final { scan-tree-dump-not "__builtin_GOMP_alloc" "gimple" } } */
/* { dg-final { scan-tree-dump-not "__builtin_GOMP_free" "gimple" } } */

int hardcoded_alloc(int p)
{
  int a = p;
  #pragma omp allocate(a) allocator(omp_default_mem_alloc)
  return a;
}

int converted_expr_alloc(int p)
{
  int a = p;
  #pragma omp allocate(a) allocator(omp_allocator_handle_t(1))
  return a;
}

int simple_expr_alloc(int p)
{
  int a = p;
  #pragma omp allocate(a) allocator(omp_allocator_handle_t(0+1))
  return a;
}

template<omp_allocator_handle_t Alloc>
int nttp_alloc(int p)
{
  int a = p;
  #pragma omp allocate(a) allocator(Alloc)
  return a;
}
template int nttp_alloc<omp_default_mem_alloc>(int);


