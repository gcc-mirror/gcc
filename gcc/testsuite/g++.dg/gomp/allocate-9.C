/* { dg-do compile { target c++23 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include "allocate-allocator-handle.h"

/* Test allocator clause is conditionally manifestly constant evaluated.

   This test needs some work to actually detect if it is doing the right thing
   in the static cases, but we barely implement allocation for those cases so
   there isn't really much to detect.
   
   For the non-static cases, it would be ideal if we checked whether it
   optimizes properly, currently that is not being done.  */


/* Note, in practice this isn't actually useful as user handles are (AFAIK)
   assigned by the runtime, this is just for testing.  */
constexpr omp_allocator_handle_t get_alloc_fn_foldable ()
{
  if consteval {
    return omp_large_cap_mem_alloc;
  }
  else {
    return omp_allocator_handle_t{128};
  }
}

omp_allocator_handle_t my_global_user_handle = omp_allocator_handle_t{256};

constexpr omp_allocator_handle_t get_alloc_fn_non_foldable ()
{
  if consteval {
    return omp_large_cap_mem_alloc;
  }
  else {
    return my_global_user_handle;
  }
}


/* This could optimize to 128 but is not required to.  */
int non_manifest_cx_foldable (int p)
{
  int non_static_a = p;
  #pragma omp allocate(non_static_a) allocator(get_alloc_fn_foldable ())
  return non_static_a;
}
/* { dg-final { scan-tree-dump-times "= get_alloc_fn_foldable" 1 "gimple" } } */

/* At best this can optimize to 'my_global_user_handle'.  */
int non_manifest_cx_non_foldable (int p)
{
  int non_static_b = p;
  #pragma omp allocate(non_static_b) allocator(get_alloc_fn_non_foldable ())
  return non_static_b;
}
/* { dg-final { scan-tree-dump-times "= get_alloc_fn_non_foldable" 1 "gimple" } } */

/* In both of these cases, the allocator clause's expr is
   manifestly constant evaluated.  */

/* This MUST fold to omp_large_cap_mem_alloc.  */
int manifest_cx_foldable (int p)
{
  static int static_a = p;
  #pragma omp allocate(static_a) allocator(get_alloc_fn_foldable ())
  return static_a;
}

/* This MUST fold to omp_large_cap_mem_alloc, calling get_alloc_fn_non_foldable
   is intentional, the function is only non-foldable if the allocator clause
   is not manifestly constant evaluated.  */
int manifest_cx_non_foldable (int p)
{
  static int static_b = p;
  #pragma omp allocate(static_b) allocator(get_alloc_fn_non_foldable ())
  return static_b;
}
