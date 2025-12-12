// { dg-do compile }
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

typedef struct omp_alloctrait_t
{
//  omp_alloctrait_key_t key;
//  omp_uintptr_t value;
} omp_alloctrait_t;


template<typename TH, TH alloc>
void f()
{
 #pragma omp target uses_allocators(alloc)
   ;
}

template<typename TH, typename TT>
void g(TT trait, TH alloc)
{
 #pragma omp target uses_allocators(alloc)
   ;
 #pragma omp target uses_allocators(alloc(trait))
   // { dg-error "traits array 'trait' must be defined in same scope as the construct on which the clause appears" "" { target *-*-* } .-1 }
   // { dg-error "traits array 'trait' must be of 'const omp_alloctrait_t \\\[\\\]' type" "" { target *-*-* } .-2 }
   ;
}

void use()
{
  omp_allocator_handle_t my;
  static const omp_alloctrait_t t[] = {};

  f<omp_allocator_handle_t, omp_null_allocator>(); // OK
  f<omp_allocator_handle_t, omp_default_mem_alloc>(); // OK

  g<omp_allocator_handle_t, const omp_alloctrait_t[]>(t, my); // 't'/traits not in the same scope
}

template<typename TH, TH alloc>
void f2()
{
 #pragma omp target uses_allocators(alloc)
   // { dg-error "allocator '\\(omp_allocator_handle_t\\)300' must be either a variable or a predefined allocator" "" { target *-*-* } .-1 }
   ;
}

template<typename TH, typename TT>
void g2(TH alloc)
{
 TT t = {};
 #pragma omp target uses_allocators(alloc(t))
   ;
}

void use2()
{
  omp_allocator_handle_t my;
  const omp_allocator_handle_t wrong = (omp_allocator_handle_t) 300;

  f2<omp_allocator_handle_t, wrong>(); // 300 is not a predefined allocator

  g2<omp_allocator_handle_t, const omp_alloctrait_t[]>(my); // OK
}

// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 51 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 58 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 89 }
