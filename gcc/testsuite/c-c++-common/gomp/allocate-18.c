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
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

void test0 ()
{
  int A1[5], B1[5];
  #pragma omp allocate(A1) align(128) allocator(omp_default_mem_alloc)
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

  #ifndef __cplusplus
    _Static_assert (_Alignof(A1) == _Alignof(B1), "wrong alignment");
  #elif __cplusplus >= 201103L
     static_assert (alignof(A1) == alignof(B1), "wrong alignment"); 
  #endif
}

void
test1 ()
{
  int x[5];
  #pragma omp parallel allocate(omp_thread_mem_alloc: x) firstprivate(x)
   x[0] = 1;

  #pragma omp target allocate(omp_thread_mem_alloc: x) firstprivate(x) /* uses_allocators(omp_thread_mem_alloc) */
   /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'target' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
   x[0] = 1;

  #pragma omp taskloop allocate(omp_thread_mem_alloc: x) firstprivate(x)
   /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'taskloop' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
   for (int i = 0; i < 5; i++)
     x[i] = i;

  #pragma omp parallel master taskloop simd allocate(omp_thread_mem_alloc: x) firstprivate(x)
   /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'taskloop' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
   for (int i = 0; i < 5; i++)
     x[i] = i;

  #pragma omp parallel
  #pragma omp masked
  {
    #pragma omp task allocate(omp_thread_mem_alloc: x) firstprivate(x)
      /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'task' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
      x[0] = 1;
  }
}
