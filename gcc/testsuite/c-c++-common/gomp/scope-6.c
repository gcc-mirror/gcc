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

int a = 0, b = 42, c = 0;

void
foo (omp_allocator_handle_t h)
{
  #pragma omp scope private (a) private (b) reduction (+: c) allocate (allocator (h): a, b, c)
  {
    if (b != 42)
      __builtin_abort ();
    a = 36;
    b = 15;
    c++;
  }
}
