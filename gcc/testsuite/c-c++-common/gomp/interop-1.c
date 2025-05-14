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

void f()
{
  constexpr omp_interop_fr_t ifr_scalar = omp_ifr_hsa;
  constexpr omp_interop_fr_t ifr_array[] = {omp_ifr_cuda, omp_ifr_hip};
  constexpr char my_string[] = "cuda";
  omp_interop_t obj1, obj2, obj3, obj4, obj5;
  int x;

  #pragma omp interop init(targetsync: obj1) init(target,targetsync : obj2, obj3) nowait   // OK
  #pragma omp interop init(target: obj1) init (targetsync  : obj2, obj3) nowait   // OK
  #pragma omp interop init(target: obj1) init (targetsync , target : obj2, obj3) nowait   // OK

  #pragma omp interop init(target: obj1) init(target,targetsync,target: obj2, obj3) nowait   // { dg-error "duplicate 'target' modifier" }
  #pragma omp interop init(target: obj1) init(target,targetsync, targetsync : obj2, obj3) nowait   // { dg-error "duplicate 'targetsync' modifier" }

  #pragma omp interop init(prefer_type("cuda", omp_ifr_opencl, omp_ifr_level_zero, "hsa"), targetsync : obj1) \
                      destroy(obj2, obj3) depend(inout: x) use(obj4, obj5) device(device_num: 0)

  #pragma omp interop init(prefer_type("cu" "da"), targetsync : obj1)   // OK

  #pragma omp assume contains(interop)
  {
  #pragma omp interop init(target, prefer_type("cuða") : obj3)  // { dg-warning "unknown foreign runtime identifier 'cu\[^'\]*a'" }
  }

#pragma omp interop init(target, prefer_type("cu\0da") : obj3) // { dg-error "string literal must not contain '\\\\0'" }

  #pragma omp interop depend(inout: x) , use(obj2), destroy(obj3) //  OK, use or destroy might have 'targetsync'

  #pragma omp interop depend(inout: x) use(obj2), destroy(obj3) //  Likewise

  #pragma omp interop depend(inout: x) init(targetsync : obj5)  use(obj2), destroy(obj3) init(prefer_type("cuda"), targetsync : obj4) // OK

  #pragma omp interop init ( target , prefer_type( { fr("hsa") }, "hip") : obj1) // { dg-error "expected '\{' before string constant" }

  #pragma omp interop init ( target , prefer_type( { fr("hsa"), attr("ompx_nothing") , fr("hsa" ) }) :obj1) // { dg-error "duplicated 'fr' preference selector before '\\(' token" }

  #pragma omp interop init (target,  prefer_type( 4, omp_ifr_hip*4) : obj1)  // { dg-warning "unknown foreign runtime identifier '20'" }
  #pragma omp interop init (prefer_type( __builtin_sin(3.3), target : obj1)
  // { dg-error "expected string literal or constant integer expression" "" { target *-*-* } .-1 }

#pragma omp interop init (prefer_type( __builtin_sin(3.3)), target : obj1)  // { dg-error "expected string literal or constant integer expression before '\\)' token" }
  #pragma omp interop init (target, prefer_type( {fr(4 ) }) : obj1) // OK
  #pragma omp interop init (target, prefer_type( {fr("cu\0da" ) }) : obj1) // { dg-error "string literal must not contain '\\\\0'" }
  #pragma omp interop init (target, prefer_type( {fr("cuda\0") }) : obj1) // { dg-error "string literal must not contain '\\\\0'" }
  #pragma omp interop init (target, prefer_type( {fr("cuda" ) }) : obj1) // OK
  #pragma omp interop init (target, prefer_type( {fr(omp_ifr_level_zero ) }, {fr(omp_ifr_hip)}) : obj1) // OK
  #pragma omp interop init (target, prefer_type( {fr("cuda",  "cuda_driver") }) : obj1) // { dg-error "60: expected '\\)' before ',' token" }
  #pragma omp interop init (target, prefer_type( {fr(my_string) }) : obj1) // { dg-error "63: expected string literal or constant integer expression before '\\)' token" }
  #pragma omp interop init (target, prefer_type( {fr("hello" }) : obj1) // { dg-error "expected '\\)' before '\}' token" }
  /* { dg-warning "unknown foreign runtime identifier 'hello' \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }  */
  #pragma omp interop init (target, prefer_type( {fr("hello") }) : obj1)
  /* { dg-warning "unknown foreign runtime identifier 'hello' \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }  */

  #pragma omp interop init (target, prefer_type( {fr(x) }) : obj1) // { dg-error "expected string literal or constant integer expression before '\\)' token" }

  #pragma omp interop init (target, prefer_type( {fr(ifr_scalar ) }) : obj1) // OK
  #pragma omp interop init (target, prefer_type( {fr(ifr_array ) }) : obj1) // { dg-error "expected string literal or constant integer expression before '\\)' token" }
  // OK in C++, for C: constexpr arrays are not part of C23; however, they are/were under consideration for C2y.
  #pragma omp interop init (target, prefer_type( {fr(ifr_array[0] ) }) : obj1)
  // { dg-error "expected string literal or constant integer expression before '\\)' token" "" { target c } .-1 }

  #pragma omp interop init (target, prefer_type( omp_ifr_level_zero, omp_ifr_hip ) : obj1) // OK
  #pragma omp interop init (target, prefer_type( omp_ifr_level_zero +1 ) : obj1) // OK
  #pragma omp interop init (target, prefer_type( x ) : obj1) // { dg-error "expected string literal or constant integer expression before '\\)' token" }

  #pragma omp interop init (target, prefer_type( ifr_scalar ) : obj1) // OK
  #pragma omp interop init (target, prefer_type( ifr_array ) : obj1) // { dg-error "expected string literal or constant integer expression before '\\)' token" }
  // OK in C++, for C: constexpr arrays are not part of C23; however, they are/were under consideration for C2y.
  #pragma omp interop init (target, prefer_type( ifr_array[1] ) : obj1)
  // { dg-error "expected string literal or constant integer expression before '\\)' token" "" { target c } .-1 }

  #pragma omp interop init (target, prefer_type( 4, omp_ifr_hip*4) : obj1) // { dg-warning "unknown foreign runtime identifier '20'" }
  #pragma omp interop init (target, prefer_type( 4, 1, 3) : obj1)

  #pragma omp interop init (target, prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }) : obj1)
  #pragma omp interop init (target, prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa,omp_ifr_level_zero)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }) : obj1) // { dg-error "80: expected '\\)' before ',' token" }
  #pragma omp interop init (target, prefer_type( {fr("cuda",5) }, {fr(omp_ifr_hsa,omp_ifr_level_zero)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }) : obj1) // { dg-error "60: expected '\\)' before ',' token" }
  #pragma omp interop init (target, prefer_type( {fr("sycl"), attr("ompx_1", "ompx_2"), attr("ompx_3") }, {attr("ompx_4", "ompx_5"),fr(omp_ifr_level_zero)} ) : obj1)
  #pragma omp interop init (target, prefer_type( { fr(5), attr("ompx_1") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } ) : obj1)
}
