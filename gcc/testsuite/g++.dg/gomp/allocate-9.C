#include "allocate-allocator-handle.h"

/* If the following fails because of added predefined allocators, please update
   - include/gomp-constants.h's GOMP_OMP_PREDEF_ALLOC_MAX or GOMP_OMPX_PREDEF_ALLOC_MAX
   - libgomp/env.c's parse_allocator
   - libgomp/libgomp.texi (document the new values - multiple locations)
   - gcc/testsuite/c-c++-common/gomp/allocate-9.c (fix the hardcoded values)
   - gcc/testsuite/g++.dg/gomp/allocate-8.C (update GOMP_OMP_PREDEF_ALLOC_MAX or GOMP_OMPX_PREDEF_ALLOC_MAX)
   + ensure that the memory-spaces are also up to date. */

/* I had wanted to simply include /include/gomp-constants.h to ensure
   synchronization, while also having hardcoded values as a canary, but
   including files from that directory does not seem to be supported.  */
#define GOMP_OMP_PREDEF_ALLOC_MAX	8
#define GOMP_OMPX_PREDEF_ALLOC_MIN	200
#define GOMP_OMPX_PREDEF_ALLOC_MAX	200

int g0 = 42; /* { dg-note "'g0' declared here" }*/
#pragma omp allocate(g0) allocator(omp_null_allocator)
/* { dg-error "'allocator' clause requires a predefined allocator as 'g0' is static" "" { target *-*-* } .-1 } */
int g1 = 42; /* { dg-note "'g1' declared here" }*/
#pragma omp allocate(g1) allocator(static_cast<omp_allocator_handle_t>(GOMP_OMP_PREDEF_ALLOC_MAX + 1)) 
/* { dg-error "'allocator' clause requires a predefined allocator as 'g1' is static" "If this test fails because of added predefined allocators please ensure everything is updated accordingly, see this test case for more information" { target *-*-* } .-1 } */
int g2 = 42; /* { dg-note "'g2' declared here" }*/
#pragma omp allocate(g2) allocator(static_cast<omp_allocator_handle_t>(GOMP_OMPX_PREDEF_ALLOC_MIN - 1))
/* { dg-error "'allocator' clause requires a predefined allocator as 'g2' is static" "" { target *-*-* } .-1 } */
int g3 = 42; /* { dg-note "'g3' declared here" }*/
#pragma omp allocate(g3) allocator(static_cast<omp_allocator_handle_t>(GOMP_OMPX_PREDEF_ALLOC_MAX + 1))
/* { dg-error "'allocator' clause requires a predefined allocator as 'g3' is static" "If this test fails because of added predefined allocators please ensure everything is updated accordingly, see this test case for more information" { target *-*-* } .-1 } */

void test_predefined_allocs()
{
  static int a0 = 42; /* { dg-note "'a0' declared here" }*/
  #pragma omp allocate(a0) allocator(omp_null_allocator)
  /* { dg-error "'allocator' clause requires a predefined allocator as 'a0' is static" "" { target *-*-* } .-1 } */
  static int a1 = 42; /* { dg-note "'a1' declared here" }*/
  #pragma omp allocate(a1) allocator(static_cast<omp_allocator_handle_t>(GOMP_OMP_PREDEF_ALLOC_MAX + 1))
  /* { dg-error "'allocator' clause requires a predefined allocator as 'a1' is static" "If this test fails because of added predefined allocators please ensure everything is updated accordingly, see this test case for more information" { target *-*-* } .-1 } */
  static int a2 = 42; /* { dg-note "'a2' declared here" }*/
  #pragma omp allocate(a2) allocator(static_cast<omp_allocator_handle_t>(GOMP_OMPX_PREDEF_ALLOC_MIN - 1))
  /* { dg-error "'allocator' clause requires a predefined allocator as 'a2' is static" "" { target *-*-* } .-1 } */
  static int a3 = 42; /* { dg-note "'a3' declared here" }*/
  #pragma omp allocate(a3) allocator(static_cast<omp_allocator_handle_t>(GOMP_OMPX_PREDEF_ALLOC_MAX + 1))
  /* { dg-error "'allocator' clause requires a predefined allocator as 'a3' is static" "If this test fails because of added predefined allocators please ensure everything is updated accordingly, see this test case for more information" { target *-*-* } .-1 } */
}
