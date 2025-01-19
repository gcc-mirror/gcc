/* { dg-prune-output "sorry, unimplemented: '#pragma omp interop' not yet supported" }  */

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
  omp_interop_t obj1, obj2, obj3, obj4, obj5;
  omp_interop_t target, targetsync, prefer_type;
  int x;

  #pragma omp interop init(obj1) init(target,targetsync : obj2, obj3) nowait

  #pragma omp interop init(prefer_type("cuda", omp_ifr_opencl, omp_ifr_level_zero, "hsa"), targetsync : obj1) \
                      destroy(obj2, obj3) depend(inout: x) use(obj4, obj5) device(device_num: 0)

  #pragma omp assume contains(interop)
    {
    #pragma omp interop init(prefer_type("cu da") : obj3)  // { dg-warning "unknown foreign runtime identifier 'cu da'" }
    }

  #pragma omp interop init(obj1, obj2, obj1), use(obj4) destroy(obj4)
  // { dg-error "'obj4' appears more than once in action clauses" "" { target *-*-* } .-1 }
  // { dg-error "'obj1' appears more than once in action clauses" "" { target *-*-* } .-2 }

  #pragma omp interop depend(inout: x)  // { dg-error "'depend' clause requires action clauses with 'targetsync' interop-type" }

  #pragma omp interop depend(inout: x) , use(obj2), destroy(obj3) //  OK, use or destroy might have 'targetsync'

  #pragma omp interop depend(inout: x) use(obj2), destroy(obj3) //  Likewise

  #pragma omp interop depend(inout: x) use(obj2), destroy(obj3) init(obj4) // { dg-error "'depend' clause requires action clauses with 'targetsync' interop-type" }
  // { dg-note "69: 'init' clause lacks the 'targetsync' modifier" "" { target c } .-1 }
  // { dg-note "70: 'init' clause lacks the 'targetsync' modifier" "" { target c++ } .-2 }

  #pragma omp interop depend(inout: x) init(targetsync : obj5)  use(obj2), destroy(obj3) init(obj4) // { dg-error "'depend' clause requires action clauses with 'targetsync' interop-type" }
  // { dg-note "'init' clause lacks the 'targetsync' modifier" "" { target *-*-* } .-1 }
  #pragma omp interop depend(inout: x) init(targetsync : obj5)  use(obj2), destroy(obj3) init(prefer_type("cuda"), targetsync : obj4) // OK

  #pragma omp interop init(target, targetsync, prefer_type, obj1)
  #pragma omp interop init(prefer_type, obj1, target, targetsync)

// Duplicated variable name or duplicated modifier:
  #pragma omp interop init(target, targetsync,target : obj1)  // { dg-error "duplicate 'target' modifier" }
  #pragma omp interop init(target, targetsync,target)         // { dg-error "'target' appears more than once in action clauses" }
  #pragma omp interop init(target : target, targetsync,target)  // { dg-error "'target' appears more than once in action clauses" }

  #pragma omp interop init(target, targetsync,targetsync : obj1)  // { dg-error "duplicate 'targetsync' modifier" }
  #pragma omp interop init(target, targetsync,targetsync)         // { dg-error "targetsync' appears more than once in action clause" }
  #pragma omp interop init(target : target, targetsync,targetsync)  // { dg-error "targetsync' appears more than once in action clause" }

  #pragma omp interop init(, targetsync, prefer_type, obj1, target)
  // { dg-error "expected identifier before ',' token" "" { target c } .-1 }
  // { dg-error "expected unqualified-id before ',' token" "" { target c++ } .-2 }
}
