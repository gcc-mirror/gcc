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
  __ompx_last_mem_alloc = omp_thread_mem_alloc,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;


static int A[5] = {1,2,3,4,5};
static int A2[5] = {1,2,3,4,5};
static int A3[5] = {1,2,3,4,5};
static int A4[5] = {1,2,3,4,5};
static int A5[5] = {1,2,3,4,5};
int B, C, C2, D;

/* If the following fails because of added predefined allocators, please update
   - c/c-parser.c's c_parser_omp_allocate
   - fortran/openmp.cc's is_predefined_allocator
   - libgomp/env.c's parse_allocator
   - libgomp/libgomp.texi (document the new values - multiple locations)
   + ensure that the memory-spaces are also up to date. */

#pragma omp allocate(A) align(32) allocator((omp_allocator_handle_t) 9) /* { dg-error "'allocator' clause requires a predefined allocator as 'A' is static" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

// typo in allocator name:
#pragma omp allocate(A2) allocator(omp_low_latency_mem_alloc)
/* { dg-error "'omp_low_latency_mem_alloc' undeclared here \\(not in a function\\); did you mean 'omp_low_lat_mem_alloc'\\?" "" { target c } .-1 } */
/* { dg-error "'omp_low_latency_mem_alloc' was not declared in this scope; did you mean 'omp_low_lat_mem_alloc'\\?" "" { target c++ } .-2 } */
/* { dg-error "'allocator' clause required for static variable 'A2'" "" { target c } .-3 } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-4 } */

/* align be const multiple of 2 */
#pragma omp allocate(A3) align(31) allocator(omp_default_mem_alloc) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */


/* allocator missing (required as A is static) */
#pragma omp allocate(A4) align(32) /* { dg-error "'allocator' clause required for static variable 'A4'" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

/* "expression in the clause must be a constant expression that evaluates to one of the
   predefined memory allocator values -> omp_low_lat_mem_alloc"  */
#pragma omp allocate(B) allocator((omp_allocator_handle_t) (omp_high_bw_mem_alloc+1)) align(32) /* OK: omp_low_lat_mem_alloc */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */


#pragma omp allocate(C) allocator((omp_allocator_handle_t) 2) /* OK: omp_large_cap_mem_alloc */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */


#pragma omp allocate(A5) align(32) allocator(omp_null_allocator) /* { dg-error "'allocator' clause requires a predefined allocator as 'A5' is static" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */

#pragma omp allocate(C2) align(32) allocator(omp_large_cap_mem_alloc)
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */


// allocate directive in same TU
int f()
{
  #pragma omp allocate(D) align(32) allocator(omp_large_cap_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'D'" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
/* { dg-note "declared here" "" { target c } 25 } */
  return A[0];
}

int g()
{
  int a2=1, b2=2;
  #pragma omp allocate(a2)
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  #pragma omp allocate(a2)  /* { dg-error "'a2' already appeared as list item in an 'allocate' directive" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  {
    int c2=3;
    #pragma omp allocate(c2, b2) /* { dg-error "'allocate' directive must be in the same scope as 'b2'" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
/* { dg-note "declared here" "" { target c } .-9 } */
    return c2+a2+b2;
  }
}

int h(int q)
{
  #pragma omp allocate(q)  /* { dg-error "function parameter 'q' may not appear as list item in an 'allocate' directive" "" { xfail c++ } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  return q;
}

int
k ()
{
  static int var3 = 8;
  #pragma omp allocate(var3) allocator((omp_allocator_handle_t)-1L)  /* { dg-error "'allocator' clause requires a predefined allocator as 'var3' is static" "" { target c } } */
/* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } .-1 } */
  return var3;
}
