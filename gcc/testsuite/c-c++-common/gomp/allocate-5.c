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
  omp_allocator_handle_t my_allocator = omp_default_mem_alloc;
  int a, b;
  static int c;
#pragma omp allocate (a)  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } } */
#pragma omp allocate (b) allocator(my_allocator)  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } } */
#pragma omp allocate(c) align(32)
  /* { dg-message "'allocator' clause required for static variable 'c'" "" { target c } .-1 } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-2 } */
}

void
bar ()
{
  int a, a2, b;
  omp_allocator_handle_t my_allocator;
#pragma omp allocate  /* { dg-error "expected '\\(' before end of line" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
#pragma omp allocate allocator(my_allocator)  /* { dg-error "expected '\\(' before 'allocator'" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
#pragma omp allocate(a) foo(my_allocator) /* { dg-error "expected 'allocator'" } */
  /* { dg-error "expected end of line before '\\(' token" "" { target *-*-* } .-1 } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-2 } */
#pragma omp allocate(a2) allocator(b)  /* { dg-error "'allocator' clause allocator expression has type 'int' rather than 'omp_allocator_handle_t'" "todo: cp/semantics.c" { xfail c++ } } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
}


void
align_test ()
{
  int i1,i2,i3,i4,i5,i6;
  #pragma omp allocate(i1) allocator(omp_default_mem_alloc), align(32)
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  #pragma omp allocate(i2) align ( 32 ),allocator(omp_default_mem_alloc)
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  #pragma omp allocate(i3),allocator(omp_default_mem_alloc) align(32)
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  #pragma omp allocate(i4) align ( 32 ) allocator(omp_default_mem_alloc)
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

  #pragma omp allocate(i5) allocator ( omp_high_bw_mem_alloc ), align ( 32 ) allocator(omp_default_mem_alloc)
  /* { dg-error "too many 'allocator' clauses" "" { target *-*-* } .-1 } */
  /* { dg-error "expected end of line before '\\)' token" "" { target *-*-* } .-2 } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-3 } */
  #pragma omp allocate(i6) align ( 32 ), align(32) allocator(omp_default_mem_alloc)
  /* { dg-error "too many 'align' clauses" "" { target *-*-* } .-1 } */
  /* { dg-error "expected end of line before '\\)' token" "" { target *-*-* } .-2 } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-3 } */
}

void
align_test2 ()
{
  int i, i2,i3;
  #pragma omp allocate(i) align (32.0)  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  #pragma omp allocate(i2) align ( 31 )  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  #pragma omp allocate(i3) align ( -32 )  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
}
