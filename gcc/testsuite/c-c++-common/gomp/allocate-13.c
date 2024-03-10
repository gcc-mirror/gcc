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

void
f ()
{
  int i;
  #pragma omp parallel firstprivate(i) \
        allocate(allocator(omp_low_latency_mem_alloc): i)
/* { dg-error "'omp_low_latency_mem_alloc' undeclared \\(first use in this function\\); did you mean 'omp_low_lat_mem_alloc'\\\?" "" { target c } .-1 } */
/* { dg-error "'omp_low_latency_mem_alloc' was not declared in this scope; did you mean 'omp_low_lat_mem_alloc'\\\?" "" { target c++ } .-2 } */
/* { dg-note "each undeclared identifier is reported only once for each function it appears in" "" { target c } .-3 } */
   i = 4;
}
