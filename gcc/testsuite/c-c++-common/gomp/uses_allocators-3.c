/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" } */

#include <omp.h>

int main (void)
{
  omp_allocator_handle_t memspace, traits;
  const omp_alloctrait_t mytraits[] = { { omp_atk_pinned,    omp_atv_true },
					{ omp_atk_partition, omp_atv_nearest } };
  #pragma omp target uses_allocators (memspace)
    ;
  #pragma omp target uses_allocators (traits)
    ;
  #pragma omp target uses_allocators (traits, memspace)
    ;
  #pragma omp target uses_allocators (traits (mytraits))
    ;
  #pragma omp target uses_allocators (memspace (mytraits), omp_default_mem_alloc)
    ;
  return 0;
}

/* { dg-final { scan-tree-dump "pragma omp target private\\(memspace\\) uses_allocators\\(memspace: memspace\\(\\), traits\\(\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(traits\\) uses_allocators\\(traits: memspace\\(\\), traits\\(\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(memspace\\) uses_allocators\\(memspace: memspace\\(\\), traits\\(\\)\\) private\\(traits\\) uses_allocators\\(traits: memspace\\(\\), traits\\(\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(traits\\) uses_allocators\\(traits: memspace\\(\\), traits\\(mytraits\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(memspace\\) uses_allocators\\(memspace: memspace\\(\\), traits\\(mytraits\\)\\)" "original" } } */

/* { dg-final { scan-tree-dump-times "__builtin_omp_init_allocator" 6 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_omp_destroy_allocator" 6 "gimple" } } */
