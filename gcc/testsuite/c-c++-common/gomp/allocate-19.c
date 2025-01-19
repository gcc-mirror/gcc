typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
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
  ompx_gnu_pinned_bogus_1 = 9,
  ompx_gnu_pinned_bogus_2 = 199,
  ompx_gnu_pinned_mem_alloc = 200,
  ompx_gnu_pinned_bogus_3 = 2001,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

static int A1[5] = {1,2,3,4,5}, B1[5];
#pragma omp allocate(A1) align(128) allocator(omp_default_mem_alloc)
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

#ifndef __cplusplus
_Static_assert (_Alignof(A1) == _Alignof(B1), "wrong alignment");
#elif __cplusplus >= 201103L
static_assert (alignof(A1) == alignof(B1), "wrong alignment");
#endif


static int *ptr;
#pragma omp allocate(ptr) align(2) allocator(omp_default_mem_alloc)
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

#ifndef __cplusplus
_Static_assert (_Alignof(ptr) == _Alignof(int*), "wrong alignment");
#elif __cplusplus >= 201103L
static_assert (alignof(ptr) == alignof(int*), "wrong alignment");
#endif


int *
get ()
{
  static int q = 0;
  #pragma omp allocate(q) align(1024) allocator(omp_default_mem_alloc)
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

#ifndef __cplusplus
  _Static_assert (_Alignof(q) == _Alignof(int), "wrong alignment");
#elif __cplusplus >= 201103L
  static_assert (alignof(q) == alignof(int), "wrong alignment");
#endif

  q += 1;
  return &A1[q];
}

static int invalid1, okay1, invalid2, invalid3;
#pragma omp allocate(invalid1) align(128) allocator(ompx_gnu_pinned_bogus_1) /* { dg-error "'allocator' clause requires a predefined allocator as 'invalid1' is static" "" { xfail c++ } }  */
#pragma omp allocate(okay1) align(128) allocator(ompx_gnu_pinned_mem_alloc)  /* Okay */
#pragma omp allocate(invalid2) align(128) allocator(ompx_gnu_pinned_bogus_2) /* { dg-error "'allocator' clause requires a predefined allocator as 'invalid2' is static" "" { xfail c++ } }  */
#pragma omp allocate(invalid3) align(128) allocator(ompx_gnu_pinned_bogus_3) /* { dg-error "'allocator' clause requires a predefined allocator as 'invalid3' is static" "" { xfail c++ } }  */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-4 } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-4 } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-4 } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-4 } */
