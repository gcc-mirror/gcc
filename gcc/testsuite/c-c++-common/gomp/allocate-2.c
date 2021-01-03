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

int bar (int, int *, int);
omp_allocator_handle_t baz (void);

void
foo (int x, int z)
{
  int i;
  #pragma omp task allocate (x)		/* { dg-error "'x' specified in 'allocate' clause but not in an explicit privatization clause" } */
  bar (x, &x, 0);
  #pragma omp taskwait
  #pragma omp parallel allocate (x)	/* { dg-error "'x' specified in 'allocate' clause but not in an explicit privatization clause" } */
  bar (x, &x, 0);
  #pragma omp parallel for simd private (x) allocate (x)	/* { dg-error "'x' specified in 'allocate' clause but not in an explicit privatization clause" } */
  for (i = 0; i < 16; i++)
    x = i;
  #pragma omp parallel allocate (foo)	/* { dg-error "'\[^\n\r]*foo\[^\n\r]*' is not a variable in 'allocate' clause" } */
  ;
  #pragma omp parallel allocate (x) shared (x)	/* { dg-error "'x' specified in 'allocate' clause but not in an explicit privatization clause" } */
  bar (x, &x, 0);
  #pragma omp parallel private (x) allocate (x) allocate (x)	/* { dg-warning "'x' appears more than once in 'allocate' clauses" } */
  bar (x, &x, 0);
  #pragma omp parallel private (x) allocate (x, x)	/* { dg-warning "'x' appears more than once in 'allocate' clauses" } */
  bar (x, &x, 0);
  #pragma omp parallel private (x) allocate (0.0 : x)	/* { dg-error "'allocate' clause allocator expression has type 'double' rather than 'omp_allocator_handle_t'" } */
  bar (x, &x, 0);
  #pragma omp parallel private (x) allocate (0 : x)	/* { dg-error "'allocate' clause allocator expression has type 'int' rather than 'omp_allocator_handle_t'" } */
  bar (x, &x, 0);
}
