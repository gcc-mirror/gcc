#include "allocate-allocator-handle.h"

static int A1[5] = {1,2,3,4,5}; /* { dg-line A_decl } */
static int A2[5] = {1,2,3,4,5};
static int A3[5] = {1,2,3,4,5};
static int A4[5] = {1,2,3,4,5}; /* { dg-line A4_decl } */
static int A5[5] = {1,2,3,4,5}; /* { dg-line A5_decl } */
int B, C, C2, D; /* { dg-note "declared here" } */

/* If the following fails because of added predefined allocators, please update
   - include/gomp-constants.h's GOMP_OMP_PREDEF_ALLOC_MAX or GOMP_OMPX_PREDEF_ALLOC_MAX
   - c/c-parser.c's c_parser_omp_allocate
   - fortran/openmp.cc's is_predefined_allocator
   - libgomp/env.c's parse_allocator
   - libgomp/libgomp.texi (document the new values - multiple locations)
   + ensure that the memory-spaces are also up to date. */

#pragma omp allocate(A1) align(32) allocator((omp_allocator_handle_t) 9) /* { dg-error "'allocator' clause requires a predefined allocator as 'A1' is static" } */
/* { dg-note "'A1' declared here" "" { target c++ } A_decl } */
// typo in allocator name:
#pragma omp allocate(A2) allocator(omp_low_latency_mem_alloc)
/* { dg-error "'omp_low_latency_mem_alloc' undeclared here \\(not in a function\\); did you mean 'omp_low_lat_mem_alloc'\\?" "" { target c } .-1 } */
/* { dg-error "'omp_low_latency_mem_alloc' was not declared in this scope; did you mean 'omp_low_lat_mem_alloc'\\?" "" { target c++ } .-2 } */
/* { dg-error "'allocator' clause required for static variable 'A2'" "" { target c } .-3 } */

/* align be const multiple of 2 */
#pragma omp allocate(A3) align(31) allocator(omp_default_mem_alloc) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */


/* allocator missing (required as A4 is static) */
#pragma omp allocate(A4) align(32) /* { dg-error "'allocator' clause required for static variable 'A4'" } */
/* { dg-note "'A4' declared here" "" { target c++ } A4_decl } */

/* "expression in the clause must be a constant expression that evaluates to one of the
   predefined memory allocator values -> omp_low_lat_mem_alloc"  */
#pragma omp allocate(B) allocator((omp_allocator_handle_t) (omp_high_bw_mem_alloc+1)) align(32) /* OK: omp_low_lat_mem_alloc */


#pragma omp allocate(C) allocator((omp_allocator_handle_t) 2) /* OK: omp_large_cap_mem_alloc */


#pragma omp allocate(A5) align(32) allocator(omp_null_allocator) /* { dg-error "'allocator' clause requires a predefined allocator as 'A5' is static" } */
/* { dg-note "'A5' declared here" "" { target c++ } A5_decl } */

#pragma omp allocate(C2) align(32) allocator(omp_large_cap_mem_alloc)


// allocate directive in same TU
int f()
{
  #pragma omp allocate(D) align(32) allocator(omp_large_cap_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'D'" } */
  return A1[0];
}

int g()
{
  int a2=1, b2=2; /* { dg-line g_a2_b2_decl } */
  #pragma omp allocate(a2)
  #pragma omp allocate(a2) /* { dg-error "'a2' already appeared as list item in an 'allocate' directive" } */
/* { dg-note "'a2' previously appeared here" "" { target c++ } .-2 } */
  {
    int c2=3;
    #pragma omp allocate(c2, b2) /* { dg-error "'allocate' directive must be in the same scope as 'b2'" } */
/* { dg-note "declared here" "" { target *-*-* } g_a2_b2_decl } */
    return c2+a2+b2;
  }
}

int h(int q)
{
  #pragma omp allocate(q)  /* { dg-error "function parameter 'q' may not appear as list item in an 'allocate' directive" } */
/* { dg-note "parameter 'q' declared here" "" { target c++ } .-3 } */
  return q;
}

int
k ()
{
  static int var3 = 8;
  #pragma omp allocate(var3) allocator((omp_allocator_handle_t)-1L)  /* { dg-error "'allocator' clause requires a predefined allocator as 'var3' is static" } */
/* { dg-note "'var3' declared here" "" { target c++ } .-2 } */
  return var3;
}
