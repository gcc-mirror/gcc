/* This file has a syntax error but should not ICE.
   Namely, a '}' is missing in one(). */

typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  omp_default_mem_alloc = 1,
  omp_low_lat_mem_alloc = 5,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

#include <stdint.h>

void
one ()
{  /* { dg-note "to match this '\{'" "" { target c++ } } */
  int result = 0, n = 3;
  #pragma omp target map(tofrom: result) firstprivate(n)
    {
      int var = 5; //, var2[n];
      #pragma omp allocate(var) align(128) allocator(omp_low_lat_mem_alloc) /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } } */
       var = 7;
}

void
two ()
{ /* { dg-error "a function-definition is not allowed here before '\{' token" "" { target c++ } } */
  int scalar = 44;
  #pragma omp allocate(scalar)

  #pragma omp parallel firstprivate(scalar)
    scalar = 33;
}
/* { dg-error "expected declaration or statement at end of input" "" { target c } .-1 } */
/* { dg-error "expected '\}' at end of input" "" { target c++ } .-2 } */
