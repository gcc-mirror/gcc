/* { dg-do compile } */

#include <omp.h>

omp_alloctrait_key_t k;
omp_alloctrait_value_t v;

int main (void)
{
  omp_allocator_handle_t foo, bar;
  const omp_alloctrait_t traits_array[] = { { omp_atk_pinned,    omp_atv_true },
					    { omp_atk_partition, omp_atv_nearest } };

  #pragma omp target uses_allocators (baz) /* { dg-error "'baz' undeclared .first use in this function." "" { target c } } */
    ;                                      /* { dg-error "'baz' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (foo (xyz)) /* { dg-error "'xyz' undeclared .first use in this function." "" { target c } } */
    ;                                            /* { dg-error "'xyz' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (foo (traits_array), baz (traits_array)) /* { dg-error "'baz' has not been declared" "" { target c++ } } */
    ;
  #pragma omp target uses_allocators (memspace(omp_no_such_space) : foo) /* { dg-error "'omp_no_such_space' undeclared .first use in this function." "" { target c } } */
    ;                                                                    /* { dg-error "'omp_no_such_space' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (memspace(1) : foo) /* { dg-error "expected identifier before numeric constant" } */
    ;                                                    /* { dg-error "expected '\\\)' before ':' token" "" { target c } .-1 } */
  #pragma omp target uses_allocators (memspace(omp_no_such_space) : foo, bar) /* { dg-error "'uses_allocators' clause only accepts a single allocator when using modifiers" } */
    ;                                                                         /* { dg-error "'omp_no_such_space' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (traits(xyz) : bar) /* { dg-error "traits array must be of 'const omp_alloctrait_t \\\[\\\]' type" "" { target c } } */
    ;                                                    /* { dg-error "'xyz' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space), traits(traits_array), memspace (omp_no_such_space) : bar) /* { dg-error "duplicate 'memspace' modifier" } */
    ;
  #pragma omp target uses_allocators (traitz(traits_array), memspace(omp_high_bw_mem_space) : bar) /* { dg-error "unknown modifier 'traitz'" } */
    ;
  #pragma omp target uses_allocators (omp_null_allocator) /* { dg-error "'omp_null_allocator' cannot be used in 'uses_allocators' clause" } */
    ;
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo, bar) /* { dg-error "'uses_allocators' clause only accepts a single allocator when using modifiers" } */
    ;
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo(foo_traits)) /* { dg-error "legacy 'foo\\\(foo_traits\\\)' traits syntax not allowed in 'uses_allocators' clause when using modifiers" } */
    ;
  return 0;
}
