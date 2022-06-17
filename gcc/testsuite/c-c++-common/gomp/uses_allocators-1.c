/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" } */

#include <omp.h>

omp_alloctrait_key_t k;
omp_alloctrait_value_t v;

int main (void)
{
  omp_allocator_handle_t foo, bar;
  const omp_alloctrait_t foo_traits[] = { { omp_atk_pinned,    omp_atv_true },
  					  { omp_atk_partition, omp_atv_nearest } };
  #pragma omp target
    ;
  #pragma omp target uses_allocators (bar)
    ;
  #pragma omp target uses_allocators (foo (foo_traits))
    ;
  #pragma omp target uses_allocators (foo (foo_traits), bar (foo_traits))
    ;
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo)
    ;
  #pragma omp target uses_allocators (traits(foo_traits) : bar)
    ;
  #pragma omp target parallel uses_allocators (memspace(omp_high_bw_mem_space), traits(foo_traits) : bar)
    ;
  #pragma omp target parallel uses_allocators (traits(foo_traits), memspace(omp_high_bw_mem_space) : bar) uses_allocators(foo)
  {
    void *p = omp_alloc ((unsigned long) 32, bar);
    omp_free (p, bar);
  }
  return 0;
}

/* { dg-final { scan-tree-dump "pragma omp target" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(\\), traits\\(\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(foo\\) uses_allocators\\(foo: memspace\\(\\), traits\\(foo_traits\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(\\), traits\\(foo_traits\\)\\) private\\(foo\\) uses_allocators\\(foo: memspace\\(\\), traits\\(foo_traits\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(foo\\) uses_allocators\\(foo: memspace\\(omp_high_bw_mem_space\\), traits\\(\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(\\), traits\\(foo_traits\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(omp_high_bw_mem_space\\), traits\\(foo_traits\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(omp_high_bw_mem_space\\), traits\\(foo_traits\\)\\) private\\(foo\\) uses_allocators\\(foo: memspace\\(\\), traits\\(\\)\\)" "original" } } */

/* { dg-final { scan-tree-dump-times "__builtin_omp_init_allocator" 9 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_omp_destroy_allocator" 9 "gimple" } } */
