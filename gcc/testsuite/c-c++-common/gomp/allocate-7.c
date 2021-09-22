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

int bar (int *);
omp_allocator_handle_t baz (void);

void
foo (int x, int z)
{
  int i;
  #pragma omp parallel private (x) allocate (allocator (0.0) : x)	/* { dg-error "'allocate' clause allocator expression has type 'double' rather than 'omp_allocator_handle_t'" } */
  bar (&x);
  #pragma omp parallel private (x) allocate (allocator (0) : x)	/* { dg-error "'allocate' clause allocator expression has type 'int' rather than 'omp_allocator_handle_t'" } */
  bar (&x);
  #pragma omp parallel private (x) allocate (align (z) : x)	/* { dg-error "'allocate' clause 'align' modifier argument needs to be positive constant power of two integer expression" } */
  bar (&x);
  #pragma omp parallel private (x) allocate (align (16.0) : x)	/* { dg-error "'allocate' clause 'align' modifier argument needs to be positive constant power of two integer expression" } */
  bar (&x);
  #pragma omp parallel private (x) allocate (align (14) : x)	/* { dg-error "'allocate' clause 'align' modifier argument needs to be positive constant power of two integer expression" } */
  bar (&x);
  #pragma omp parallel private (x) allocate (align (0) : x)	/* { dg-error "'allocate' clause 'align' modifier argument needs to be positive constant power of two integer expression" } */
  bar (&x);
  #pragma omp parallel private (x) allocate (align (16), align (16) : x)	/* { dg-error "expected|duplicate|declared|specified" } */
  bar (&x);									/* { dg-warning "more than once" "" { target c++ } .-1 } */
  #pragma omp parallel private (x) allocate (allocator (omp_default_mem_alloc), allocator (omp_default_mem_alloc) : x)	/* { dg-error "expected|duplicate|declared|specified" } */
  bar (&x);									/* { dg-warning "more than once" "" { target c++ } .-1 } */
}
