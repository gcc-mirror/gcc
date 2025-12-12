// { dg-do compile }

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


void f()
{
 omp_allocator_handle_t my;
 struct t {
   omp_allocator_handle_t a1;
 } s;

const omp_allocator_handle_t my3 = (omp_allocator_handle_t) 300;
omp_allocator_handle_t my4[1];
const omp_alloctrait_t t[] = {};
 #pragma omp target uses_allocators(my, omp_default_mem_alloc, omp_null_allocator)  // OK
   ;
 #pragma omp target uses_allocators(my) firstprivate(my) // { dg-error "'my' appears more than once in data clauses" }
   ;
 #pragma omp target private(my) uses_allocators(my) // { dg-error "'my' appears more than once in data clauses" }
   ;
 #pragma omp target uses_allocators(my3)
   ;
 #pragma omp target uses_allocators(s.a1)
   // { dg-error "expected '\\)' before '\\.' token" "" { target *-*-* } .-1 }
   // { dg-error "allocator 's' must be of 'omp_allocator_handle_t' type" "" { target *-*-* } .-2 }
   ;
 #pragma omp target uses_allocators(my4)
   // { dg-error "allocator 'my4' must be of 'omp_allocator_handle_t' type" "" { target *-*-* } .-1 }
   ;
 #pragma omp target uses_allocators(my4[0])
   // { dg-error "expected '\\)' before '\\\[' token" "" { target *-*-* } .-1 }
   // { dg-error "allocator 'my4' must be of 'omp_allocator_handle_t' type" "" { target *-*-* } .-2 }
   ;
 #pragma omp target uses_allocators(memspace(omp_default_mem_space) : my, my(t)) // { dg-error "legacy 'my\\(t\\)' traits syntax not allowed in 'uses_allocators' clause when using modifiers" }
   ;
}

// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 57 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 61 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 63 }
// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 76 }
