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

omp_allocator_handle_t baz (int);

int
foo (omp_allocator_handle_t h1, omp_allocator_handle_t h2, int y)
{
  int x;
  #pragma omp taskloop default(none) lastprivate (x) allocate (h1:x) firstprivate(y) allocate (h2:y)
  for (int i = 0; i < 64; i++)
    x = y + i;
  return x;
}

int
bar (int y)
{
  int x;
  #pragma omp taskloop default(none) lastprivate (x) allocate (baz (0):x) allocate (baz (1):y) firstprivate(y)
  for (int i = 0; i < 64; i++)
    x = y + i;
  return x;
}
