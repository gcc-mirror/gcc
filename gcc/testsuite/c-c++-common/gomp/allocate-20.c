// { dg-do compile }

typedef __UINTPTR_TYPE__ omp_uintptr_t;

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : omp_uintptr_t
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

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

int g0;
int g1;
int g2;
int g3;
int g4;
int g5;
int g6;
int g7;
int g8;
int g9;
int g10;

#pragma omp allocate(g0) allocator(omp_null_allocator)  // { dg-error "'allocator' clause requires a predefined allocator as 'g0' is static" "" { target c } }
// { dg-error "'allocator' clause requires a constant predefined allocator" "" { target c++ } .-1 }
#pragma omp allocate(g1) allocator(omp_default_mem_alloc)
#pragma omp allocate(g2) allocator(omp_large_cap_mem_alloc)
#pragma omp allocate(g3) allocator(omp_const_mem_alloc)
#pragma omp allocate(g4) allocator(omp_high_bw_mem_alloc)
#pragma omp allocate(g5) allocator(omp_low_lat_mem_alloc)
#pragma omp allocate(g6) allocator(omp_cgroup_mem_alloc)  // { dg-error "'allocator' clause for static variable 'g6' must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c } }
// { dg-error "'allocator' clause must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c++ } .-1 }
#pragma omp allocate(g7) allocator(omp_pteam_mem_alloc)   // { dg-error "'allocator' clause for static variable 'g7' must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c } }
// { dg-error "'allocator' clause must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c++ } .-1 }
#pragma omp allocate(g8) allocator(omp_thread_mem_alloc)  // { dg-error "'allocator' clause for static variable 'g8' must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c } }
// { dg-error "'allocator' clause must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c++ } .-1 }
#pragma omp allocate(g9) allocator(ompx_gnu_pinned_mem_alloc)
#pragma omp allocate(g10) allocator(ompx_gnu_managed_mem_alloc)


void local_static ()
{
  static int s0;
  static int s1;
  static int s2;
  static int s3;
  static int s4;
  static int s5;
  static int s6;
  static int s7;
  static int s8;
  static int s9;
  static int s10;

  #pragma omp allocate(s0) allocator(omp_null_allocator)  // { dg-error "'allocator' clause requires a predefined allocator as 's0' is static" "" { target c } }
  // { dg-error "'allocator' clause requires a constant predefined allocator" "" { target c++ } .-1 }
  #pragma omp allocate(s1) allocator(omp_default_mem_alloc)
  #pragma omp allocate(s2) allocator(omp_large_cap_mem_alloc)
  #pragma omp allocate(s3) allocator(omp_const_mem_alloc)
  #pragma omp allocate(s4) allocator(omp_high_bw_mem_alloc)
  #pragma omp allocate(s5) allocator(omp_low_lat_mem_alloc)
  #pragma omp allocate(s6) allocator(omp_cgroup_mem_alloc)  // { dg-error "'allocator' clause for static variable 's6' must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c } }
  // { dg-error "'allocator' clause must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c++ } .-1 }
  #pragma omp allocate(s7) allocator(omp_pteam_mem_alloc)   // { dg-error "'allocator' clause for static variable 's7' must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c } }
  // { dg-error "'allocator' clause must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c++ } .-1 }
  #pragma omp allocate(s8) allocator(omp_thread_mem_alloc)  // { dg-error "'allocator' clause for static variable 's8' must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c } }
  // { dg-error "'allocator' clause must not be 'omp_cgroup_mem_alloc', 'omp_pteam_mem_alloc', or 'omp_thread_mem_alloc'" "" { target c++ } .-1 }
  #pragma omp allocate(s9) allocator(ompx_gnu_pinned_mem_alloc)
  #pragma omp allocate(s10) allocator(ompx_gnu_managed_mem_alloc)
}
