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
 omp_allocator_handle_t my, my2, my3, my4;
const omp_alloctrait_t t[] = {};
const omp_alloctrait_t t2[] = {};
 #pragma omp target uses_allocators(traits(t), memspace(omp_high_bw_mem_space) : my; omp_default_mem_alloc, omp_null_allocator; my2; traits(t2) : my3; memspace(omp_large_cap_mem_space) : my4)
   ;
}

// { dg-final { scan-tree-dump "#pragma omp target uses_allocators\\(memspace\\(1\\), traits\\(\\) : my4\\) uses_allocators\\(memspace\\(\\), traits\\(t2\\) : my3\\) uses_allocators\\(memspace\\(\\), traits\\(\\) : my2\\) uses_allocators\\(memspace\\(3\\), traits\\(t\\) : my\\)" "original" } }


// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 52 }
