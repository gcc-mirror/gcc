/* { dg-additional-options "-fdump-tree-original" }  */

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

void
f()
{
  omp_interop_t obj1, obj2, obj3, obj4, obj5, obj6, obj7;
  int x[6];

#pragma omp interop init (target: obj1, obj2) use (obj3) destroy(obj4) init(targetsync: obj5) destroy(obj6) use(obj7)
  /* { dg-final { scan-tree-dump-times "#pragma omp interop use\\(obj7\\) destroy\\(obj6\\) init\\(targetsync: obj5\\) destroy\\(obj4\\) use\\(obj3\\) init\\(target: obj2\\) init\\(target: obj1\\)\[\r\n\]" 1 "original" } }  */

  #pragma omp interop nowait init (targetsync : obj1, obj2) use (obj3) destroy(obj4) init(target, targetsync : obj5) destroy(obj6) use(obj7) depend(inout: x)  
  /* { dg-final { scan-tree-dump-times "#pragma omp interop depend\\(inout:x\\) use\\(obj7\\) destroy\\(obj6\\) init\\(target, targetsync: obj5\\) destroy\\(obj4\\) use\\(obj3\\) init\\(targetsync: obj2\\) init\\(targetsync: obj1\\) nowait\[\r\n\]" 1 "original" } }  */

#pragma omp interop init (target: obj1, obj2) init (target: obj3) init(targetsync : obj4) init(target,targetsync: obj5)  
  /* { dg-final { scan-tree-dump-times "#pragma omp interop init\\(target, targetsync: obj5\\) init\\(targetsync: obj4\\) init\\(target: obj3\\) init\\(target: obj2\\) init\\(target: obj1\\)\[\r\n\]" 1 "original" } }  */

  /* --------------------------------------------  */

  #pragma omp interop init (target, prefer_type(omp_ifr_cuda, omp_ifr_cuda+1, "hsa", "myPrivateInterop", omp_ifr_cuda-2) : obj1, obj2) init (target: obj3) init(prefer_type(omp_ifr_hip, "sycl", omp_ifr_opencl), targetsync : obj4, obj7) init(target,prefer_type("level_zero", omp_ifr_level_zero+0),targetsync: obj5)  
  /*
     { dg-warning "unknown foreign runtime identifier 'myPrivateInterop' \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
     { dg-warning "unknown foreign runtime identifier '-1' \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }

     { dg!final { scan-tree-dump-times "#pragma omp interop init\\(prefer_type\\({fr\\(\"level_zero\"\\)}, {fr\\(\"level_zero\"\\)}\\), target, targetsync: obj5\\) init\\(prefer_type\\({fr\\(\"hip\"\\)}, {fr\\(\"sycl\"\\)}, {fr\\(\"opencl\"\\)}\\), targetsync: obj7\\) init\\(prefer_type\\({fr\\(\"hip\"\\)}, {fr\\(\"sycl\"\\)}, {fr\\(\"opencl\"\\)}\\), targetsync: obj4\\) init\\(target: obj3\\) init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\)}, {fr\\(\"hsa\"\\)}, {fr\\(\"<unknown>\"\\)}, {fr\\(\"<unknown>\"\\)}\\), target: obj2\\) init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\)}, {fr\\(\"hsa\"\\)}, {fr\\(\"<unknown>\"\\)}, {fr\\(\"<unknown>\"\\)}\\), target: obj1\\)\[\r\n\]" 1 "original" } }
  */


/* -------------------------------------------- */

  #pragma omp interop init ( target, prefer_type( {fr("hip"), attr("ompx_gnu_prio:1", "ompx_gnu_debug")}, {attr("ompx_gnu_nicest"), attr("ompx_something")}) : obj1, obj2) init ( prefer_type( {fr("cuda")}, {fr(omp_ifr_cuda_driver), attr("ompx_nix")}, {fr("best")}), targetsync : obj3, obj4) nowait use(obj5)  
  /*
     { dg-warning "unknown foreign runtime identifier 'best' \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }

     { dg-final { scan-tree-dump-times "#pragma omp interop use\\(obj5\\) nowait init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\),attr\\(\"ompx_nix\"\\)}, {fr\\(\"<unknown>\"\\)}\\), targetsync: obj4\\) init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\),attr\\(\"ompx_nix\"\\)}, {fr\\(\"<unknown>\"\\)}\\), targetsync: obj3\\) init\\(prefer_type\\({fr\\(\"hip\"\\),attr\\(\"ompx_gnu_prio:1\"\\),attr\\(\"ompx_gnu_debug\"\\)}, {attr\\(\"ompx_gnu_nicest\"\\),attr\\(\"ompx_something\"\\)}\\), target: obj2\\) init\\(prefer_type\\({fr\\(\"hip\"\\),attr\\(\"ompx_gnu_prio:1\"\\),attr\\(\"ompx_gnu_debug\"\\)}, {attr\\(\"ompx_gnu_nicest\"\\),attr\\(\"ompx_something\"\\)}\\), target: obj1\\)\[\r\n\]" 1 "original" } }
  */

}

void
g (int *y)
{
  omp_interop_t io1, io2, io3, io4, io5;

  [[omp::directive (interop,init(prefer_type({fr("level_zero")}, {fr(omp_ifr_sycl),attr("ompx_in_order"),attr("ompx_queue:in_order")}), targetsync : io1, io2),use(io3),destroy(io4,io5),depend(inout:y),nowait)]];  

  /* { dg-final { scan-tree-dump-times "#pragma omp interop nowait depend\\(inout:y\\) destroy\\(io5\\) destroy\\(io4\\) use\\(io3\\) init\\(prefer_type\\(\{fr\\(\"level_zero\"\\)\}, \{fr\\(\"sycl\"\\),attr\\(\"ompx_in_order\"\\),attr\\(\"ompx_queue:in_order\"\\)\}\\), targetsync: io2\\) init\\(prefer_type\\(\{fr\\(\"level_zero\"\\)\}, \{fr\\(\"sycl\"\\),attr\\(\"ompx_in_order\"\\),attr\\(\"ompx_queue:in_order\"\\)\}\\), targetsync: io1\\)\[\r\n\]" 1 "original" } }  */
}
