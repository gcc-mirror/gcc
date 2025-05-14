/* { dg-do compile } */

/* At least one of the target and/or targetsync modifiers must be provided.
   This implies that there are always modifiers required, and the parser
   should reject e.g. "init (var1, var2)"; the first thing in the list is
   always an init_modifier in valid code.  */

/* The following definitions are in omp_lib, which cannot be included
   in gcc/testsuite/  */

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : __UINTPTR_TYPE__
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

typedef enum omp_interop_t __GOMP_UINTPTR_T_ENUM
{
  omp_interop_none = 0,
  __omp_interop_t_max__ = __UINTPTR_MAX__
} omp_interop_t;

typedef enum omp_interop_fr_t
{
  omp_ifr_cuda = 1,
  omp_ifr_cuda_driver = 2,
  omp_ifr_opencl = 3,
  omp_ifr_sycl = 4,
  omp_ifr_hip = 5,
  omp_ifr_level_zero = 6,
  omp_ifr_hsa = 7,
  omp_ifr_last = omp_ifr_hsa
} omp_interop_fr_t;

// ---------------------------------

void f()
{
  omp_interop_t obj1, obj2;

  #pragma omp interop init (obj1) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (obj1, obj2) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (obj1, target) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (target, obj1) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (obj1, targetsync) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (targetsync, obj1) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (targetsync, target) // { dg-error "expected ':' before '\\)' token" }

  #pragma omp interop init (target, prefer_type( {fr(4 ) }) : obj1) // OK
  #pragma omp interop init (targetsync, prefer_type( {fr(4 ) }) : obj1) // OK
  #pragma omp interop init (prefer_type( {fr(4 ) }), target : obj1) // OK

  #pragma omp interop init (prefer_type( {fr(4 ) }) : obj1) // { dg-error "missing required 'target' and/or 'targetsync' modifier" }
  #pragma omp interop init (prefer_type( {fr(4 ) }) : foobar) // { dg-error "missing required 'target' and/or 'targetsync' modifier" }  
  // { dg-error "'foobar' undeclared" "" { target c } .-1 }
  // { dg-error "'foobar' has not been declared" "" { target c++ } .-2 }
}
