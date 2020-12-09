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
foo ()
{
  int a, b;
  omp_allocator_handle_t my_allocator;
#pragma omp allocate (a)  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" } */
#pragma omp allocate (b) allocator(my_allocator)  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" } */
}

void
bar ()
{
  int a, b;
  omp_allocator_handle_t my_allocator;
#pragma omp allocate  /* { dg-error "expected '\\(' before end of line" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target *-*-* } .-1 } */
#pragma omp allocate allocator(my_allocator)  /* { dg-error "expected '\\(' before 'allocator'" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target *-*-* } .-1 } */
#pragma omp allocate(a) foo(my_allocator) /* { dg-error "expected 'allocator'" } */
  /* { dg-error "expected end of line before '\\(' token" "" { target *-*-* } .-1 } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target *-*-* } .-2 } */
#pragma omp allocate(a) allocator(b)  /* { dg-error "'allocator' clause allocator expression has type 'int' rather than 'omp_allocator_handle_t'" "todo: cp/semantics.c" { xfail c++ } } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target *-*-* } .-1 } */
}
