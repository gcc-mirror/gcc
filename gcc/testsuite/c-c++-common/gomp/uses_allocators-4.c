/* { dg-do compile } */
/* { dg-additional-options "-Wno-deprecated-openmp" } */

//#include <omp.h>

typedef __UINTPTR_TYPE__ omp_uintptr_t;

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : omp_uintptr_t
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

typedef enum omp_memspace_handle_t __GOMP_UINTPTR_T_ENUM
{
  omp_default_mem_space = 0,
  omp_large_cap_mem_space = 1,
  omp_const_mem_space = 2,
  omp_high_bw_mem_space = 3,
  omp_low_lat_mem_space = 4,
  ompx_gnu_managed_mem_space = 200,
  __omp_memspace_handle_t_max__ = __UINTPTR_MAX__
} omp_memspace_handle_t;

typedef enum omp_allocator_handle_t __GOMP_UINTPTR_T_ENUM
{
  omp_null_allocator = 0,
  omp_default_mem_alloc = 1,
  omp_large_cap_mem_alloc = 2,
  omp_const_mem_alloc = 3,
  omp_high_bw_mem_alloc = 4,
  omp_low_lat_mem_alloc = 5,
  omp_cgroup_mem_alloc = 6,
  omp_pteam_mem_alloc = 7,
  omp_thread_mem_alloc = 8,
  ompx_gnu_pinned_mem_alloc = 200,
  ompx_gnu_managed_mem_alloc = 201,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

typedef enum omp_alloctrait_key_t
{
  omp_atk_sync_hint = 1,
  omp_atk_alignment = 2,
  omp_atk_access = 3,
  omp_atk_pool_size = 4,
  omp_atk_fallback = 5,
  omp_atk_fb_data = 6,
  omp_atk_pinned = 7,
  omp_atk_partition = 8
} omp_alloctrait_key_t;

typedef enum omp_alloctrait_value_t
{
  omp_atv_default = (__UINTPTR_TYPE__) -1,
  omp_atv_false = 0,
  omp_atv_true = 1,
  omp_atv_contended = 3,
  omp_atv_uncontended = 4,
  omp_atv_serialized = 5,
  omp_atv_private = 6,
  omp_atv_all = 7,
  omp_atv_thread = 8,
  omp_atv_pteam = 9,
  omp_atv_cgroup = 10,
  omp_atv_default_mem_fb = 11,
  omp_atv_null_fb = 12,
  omp_atv_abort_fb = 13,
  omp_atv_allocator_fb = 14,
  omp_atv_environment = 15,
  omp_atv_nearest = 16,
  omp_atv_blocked = 17,
  omp_atv_interleaved = 18
} omp_alloctrait_value_t;

typedef struct omp_alloctrait_t
{
  omp_alloctrait_key_t key;
  omp_uintptr_t value;
} omp_alloctrait_t;

omp_alloctrait_key_t k;
omp_alloctrait_value_t v;

int f (const omp_alloctrait_t arg_traits[], int n)
{
  omp_allocator_handle_t foo, bar;
  const omp_alloctrait_t traits_array[] = { { omp_atk_pinned,    omp_atv_true },
					    { omp_atk_partition, omp_atv_nearest } };
  extern const omp_alloctrait_t ex_traits[2];
  extern const omp_alloctrait_t ex2_traits[];
#ifndef __cplusplus
  const omp_alloctrait_t vla_traits[n] = {};  /* Not useful, but shouldn't crash.  */
#else
  const omp_alloctrait_t vla_traits[n] = { { omp_atk_pinned,    omp_atv_true },
  					   { omp_atk_partition, omp_atv_nearest } };
#endif

  #pragma omp target uses_allocators (baz) /* { dg-error "'baz' undeclared .first use in this function." "" { target c } } */
    ;                                      /* { dg-error "'baz' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (foo (xyz)) /* { dg-error "'xyz' undeclared .first use in this function." "" { target c } } */
    ;                                            /* { dg-error "'xyz' has not been declared" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (foo (traits_array), baz (traits_array)) /* { dg-error "'baz' has not been declared" "" { target c++ } } */
    ;
  #pragma omp target uses_allocators (foo (arg_traits)) /* { dg-error "traits array 'arg_traits' must be defined in same scope as the construct on which the clause appears" } */
    ;                                                   /* { dg-error "traits array 'arg_traits' must be of 'const omp_alloctrait_t \\\[\\\]' type" "" { target *-*-* } .-1 } */
  #pragma omp target uses_allocators (foo (ex_traits)) /* { dg-error "traits array 'ex_traits' must be defined in same scope as the construct on which the clause appears" } */
    ;                                                  /* { dg-error "traits array must be initialized with constants" "" { target *-*-* } .-1 } */
  #pragma omp target uses_allocators (foo (ex2_traits)) /* { dg-error "traits array 'ex2_traits' must be defined in same scope as the construct on which the clause appears" } */
    ;                                                   /* { dg-error "traits array 'ex2_traits' must be of 'const omp_alloctrait_t \\\[\\\]' type" "" { target *-*-* } .-1 } */
  #pragma omp target uses_allocators (foo (vla_traits)) /* { dg-error "variable length traits arrays are not supported" "" { target c++ } } */
    ;
  #pragma omp target uses_allocators (memspace(omp_no_such_space) : foo) /* { dg-error "'omp_no_such_space' undeclared .first use in this function." "" { target c } } */
    ;                                                                    /* { dg-error "'omp_no_such_space' was not declared in this scope" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (memspace(1) : foo) /* { dg-error "memspace modifier '1' must be constant enum of 'omp_memspace_handle_t' type" } */
    ;
  #pragma omp target uses_allocators (memspace(omp_no_such_space) : foo, bar) /* { dg-error "'uses_allocators' clause only accepts a single allocator when using modifiers" } */
    ;                                                                         /* { dg-error "memspace modifier 'omp_no_such_space' must be constant enum of 'omp_memspace_handle_t' type" "" { target c++ } .-1 } */
  #pragma omp target uses_allocators (traits(xyz) : bar) /* { dg-error "'xyz' was not declared in this scope" "" { target c++ } } */
    ;
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space), traits(traits_array), memspace (omp_no_such_space) : bar) /* { dg-error "duplicate 'memspace' modifier" "" { target c } } */
    ;                                                                                                                            /* { dg-error "expected '\\\)' before 'memspace" "" { target c } .-1 } */
                                                                                                                                 /* { dg-error "duplicate 'memspace' modifier" "" { target c++ } .-2 } */
  #pragma omp target uses_allocators (traitz(traits_array), memspace(omp_high_bw_mem_space) : bar) /* { dg-error "'traitz' undeclared .first use in this function." "" { target c } } */
    ;                                                                                              /* { dg-error "'memspace' undeclared .first use in this function." "" { target c } .-1 } */
                                                                                                   /* { dg-error "'traitz' has not been declared" "" { target c++ } .-2 } */
                                                                                                   /* { dg-error "'memspace' has not been declared" "" { target c++ } .-3 } */
                                                                                                   /* { dg-error "expected '\\\)' before ':' token" "" { target *-*-* } .-4 } */
  #pragma omp target uses_allocators (omp_null_allocator)
    ;
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo, bar) /* { dg-error "'uses_allocators' clause only accepts a single allocator when using modifiers" } */
    ;
  #pragma omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo(foo_traits)) /* { dg-error "'foo_traits' undeclared .first use in this function.; did you mean 'vla_traits'." "" { target c } } */
    ;                                                                                    /* { dg-error "'foo_traits' has not been declared" "" { target c++ } .-1 } */
                                                                                         /* { dg-error "legacy 'foo\\\(foo_traits\\\)' traits syntax not allowed in 'uses_allocators' clause when using modifiers" "" { target *-*-* } .-2 } */
  return 0;
}

// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 103 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target c } 111 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target c } 113 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target c } 117 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target c } 119 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 131 }
