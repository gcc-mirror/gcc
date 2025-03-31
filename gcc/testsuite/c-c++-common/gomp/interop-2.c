/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23"  { target c } } */
/* C++11 and C23 because of 'constexpr'.  */

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

void f(const omp_interop_t ocp)
{
  constexpr omp_interop_t oce = omp_interop_none;
  const omp_interop_t occ = omp_interop_none;
  omp_interop_t od[5];
  omp_interop_t *op;
  short o2;
  float of;

  #pragma omp interop init (targetsync: ocp)  // { dg-error "'ocp' shall not be const" }
  #pragma omp interop init (targetsync: oce)  // { dg-error "'oce' shall not be const" }
  #pragma omp interop init (targetsync: occ)  // { dg-error "'occ' shall not be const" }
  #pragma omp interop init (targetsync: od)   // { dg-error "'od' must be of 'omp_interop_t'" }
  #pragma omp interop init (targetsync: od[1])// { dg-error "expected '\\)' before '\\\[' token" }
                                  // { dg-error "'od' must be of 'omp_interop_t'" "" { target *-*-* } .-1 }
  #pragma omp interop init (targetsync: op)   // { dg-error "'op' must be of 'omp_interop_t'" }
  #pragma omp interop init (targetsync: *op)
  // { dg-error "expected identifier before '\\*' token" "" { target c } .-1 }
  // { dg-error "expected unqualified-id before '\\*' token" "" { target c++ } .-2 }
  #pragma omp interop init (targetsync: o2)   // { dg-error "'o2' must be of 'omp_interop_t'" }
  #pragma omp interop init (targetsync: of)   // { dg-error "'of' must be of 'omp_interop_t'" }

  #pragma omp interop use (ocp)  // OK
  #pragma omp interop use (oce)  // odd but okay
  #pragma omp interop use (occ)  // okayish
  #pragma omp interop use (od)   // { dg-error "'od' must be of 'omp_interop_t'" }
  #pragma omp interop use (od[1])// { dg-error "expected '\\)' before '\\\[' token" }
                                 // { dg-error "'od' must be of 'omp_interop_t'" "" { target *-*-* } .-1 }
  #pragma omp interop use (op)   // { dg-error "'op' must be of 'omp_interop_t'" }
  #pragma omp interop use (*op)
  // { dg-error "expected identifier before '\\*' token" "" { target c } .-1 }
  // { dg-error "expected unqualified-id before '\\*' token" "" { target c++ } .-2 }
  #pragma omp interop use (o2)   // { dg-error "'o2' must be of 'omp_interop_t'" }
  #pragma omp interop use (of)   // { dg-error "'of' must be of 'omp_interop_t'" }

  #pragma omp interop destroy (ocp)  // { dg-error "'ocp' shall not be const" }
  #pragma omp interop destroy (oce)  // { dg-error "'oce' shall not be const" }
  #pragma omp interop destroy (occ)  // { dg-error "'occ' shall not be const" }
  #pragma omp interop destroy (od)   // { dg-error "'od' must be of 'omp_interop_t'" }
  #pragma omp interop destroy (od[1])// { dg-error "expected '\\)' before '\\\[' token" }
                                     // { dg-error "'od' must be of 'omp_interop_t'" "" { target *-*-* } .-1 }
  #pragma omp interop destroy (op)   // { dg-error "'op' must be of 'omp_interop_t'" }
  #pragma omp interop destroy (*op)
  // { dg-error "expected identifier before '\\*' token" "" { target c } .-1 }
  // { dg-error "expected unqualified-id before '\\*' token" "" { target c++ } .-2 }
  #pragma omp interop destroy (o2)   // { dg-error "'o2' must be of 'omp_interop_t'" }
  #pragma omp interop destroy (of)   // { dg-error "'of' must be of 'omp_interop_t'" }
}

void g()
{
  omp_interop_t obj1, obj2, obj3, obj4, obj5;
  int x;

  #pragma omp interop init (target, prefer_type( {fr("") }) : obj1)  // { dg-error "non-empty string literal expected before '\\)' token" }
  #pragma omp interop init (target, prefer_type( {fr("hip") , attr(omp_ifr_cuda) }) : obj1) ! { dg-error "expected string literal before 'omp_ifr_cuda'" }

  #pragma omp interop init (target, prefer_type( {fr("hip") , attr("myooption") }) : obj1)  // { dg-error "'attr' string literal must start with 'ompx_'" }
  #pragma omp interop init (target, prefer_type( {fr("hip") , attr("ompx_option") , attr("ompx_") } ) : obj1)
  #pragma omp interop init (target, prefer_type( {fr("hip") , attr("ompx_option") }, { attr("ompx_") } ) : obj1)
  #pragma omp interop init (target, prefer_type( {fr("hip") , attr("ompx_option") }  { attr("ompx_") } ) : obj1)  // { dg-error "expected '\\)' or ',' before '\{' token" }
  #pragma omp interop init (target, prefer_type( {fr("hip") , attr("ompx_option")   ) : obj1)  // { dg-error "expected ',' or '\}' before '\\)' token" }

  #pragma omp interop init (target, prefer_type( {fr("hip") attr("ompx_option")   ) : obj1) // { dg-error "expected ',' or '\}' before 'attr'" }
  #pragma omp interop init (target, prefer_type( {fr("hip")}), prefer_type("cuda") : obj1)  // { dg-error "duplicate 'prefer_type' modifier" }

  #pragma omp interop init (target, prefer_type( {attr("ompx_option1,ompx_option2") } ) : obj1)  // { dg-error "'attr' string literal must not contain a comma" }

  #pragma omp interop init (target, prefer_type( {attr("ompx_option1,ompx_option2")   ) : obj1) // { dg-error "'attr' string literal must not contain a comma" }

  #pragma omp interop init ( targetsync other ) : obj1)
  // { dg-error "expected an OpenMP clause before ':' token" "" { target *-*-* } .-1 }
  // { dg-error "expected ':' before 'other'" ""  { target *-*-* } .-2 }

  #pragma omp interop init (target, prefer_type( {fr("cuda") } ), other : obj1)   // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
  #pragma omp interop init (prefer_type( {fr("cuda") } ), obj1) // { dg-error "expected 'prefer_type', 'target', or 'targetsync'" }
}
