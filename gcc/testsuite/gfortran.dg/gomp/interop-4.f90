! { dg-additional-options "-fdump-tree-original" }

module m
 use iso_c_binding
 implicit none

 ! The following definitions are in omp_lib, which cannot be included
 ! in gcc/testsuite/
 integer, parameter :: omp_interop_kind = c_intptr_t
 integer, parameter :: omp_interop_fr_kind = c_int

 integer (omp_interop_kind), parameter :: omp_interop_none = 0_omp_interop_kind
 integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda = 1
 integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda_driver = 2
 integer (omp_interop_fr_kind), parameter :: omp_ifr_opencl = 3
 integer (omp_interop_fr_kind), parameter :: omp_ifr_sycl = 4
 integer (omp_interop_fr_kind), parameter :: omp_ifr_hip = 5
 integer (omp_interop_fr_kind), parameter :: omp_ifr_level_zero = 6
 integer (omp_interop_fr_kind), parameter :: omp_ifr_hsa = 7
end module m

subroutine s
use m
implicit none

integer(omp_interop_kind) :: obj1, obj2, obj3, obj4, obj5, obj6, obj7
integer :: x(6)

!$omp interop init ( obj1, obj2) use (obj3) destroy(obj4) init(obj5) destroy(obj6) use(obj7) ! { dg-message "'#pragma omp interop' not yet supported" }
! { dg-final { scan-tree-dump-times "#pragma omp interop init\\(obj1\\) init\\(obj2\\) init\\(obj5\\) use\\(obj3\\) use\\(obj7\\) destroy\\(obj4\\) destroy\\(obj6\\)\[\r\n\]" 1 "original" } }

!$omp interop nowait init (targetsync : obj1, obj2) use (obj3) destroy(obj4) init(target, targetsync : obj5) destroy(obj6) use(obj7) depend(inout: x) ! { dg-message "'#pragma omp interop' not yet supported" }
! { dg-final { scan-tree-dump-times "#pragma omp interop depend\\(inout:x\\) init\\(targetsync: obj1\\) init\\(targetsync: obj2\\) init\\(target, targetsync: obj5\\) use\\(obj3\\) use\\(obj7\\) destroy\\(obj4\\) destroy\\(obj6\\) nowait\[\r\n\]" 1 "original" } }

!$omp interop init ( obj1, obj2) init (target: obj3) init(targetsync : obj4) init(target,targetsync: obj5)  ! { dg-message "'#pragma omp interop' not yet supported" }
! { dg-final { scan-tree-dump-times "#pragma omp interop init\\(obj1\\) init\\(obj2\\) init\\(target: obj3\\) init\\(targetsync: obj4\\) init\\(target, targetsync: obj5\\)\[\r\n\]" 1 "original" } }

! --------------------------------------------

!$omp interop init (target, prefer_type(omp_ifr_cuda, omp_ifr_cuda+1, "hsa", "myPrivateInterop", omp_ifr_cuda-2) : obj1, obj2) init (target: obj3) init(prefer_type(omp_ifr_hip, "sycl", omp_ifr_opencl), targetsync : obj4, obj7) init(target,prefer_type("level_zero", omp_ifr_level_zero+0),targetsync: obj5)  ! { dg-message "'#pragma omp interop' not yet supported" }
!
! { dg-warning "Unknown foreign runtime identifier 'myPrivateInterop' at \\(1\\) \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
! { dg-warning "Unknown foreign runtime identifier '-1' at \\(1\\) \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }
!
! { dg-final { scan-tree-dump-times "#pragma omp interop init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\)}, {fr\\(\"hsa\"\\)}, {fr\\(\"<unknown>\"\\)}, {fr\\(\"<unknown>\"\\)}\\), target: obj1\\) init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\)}, {fr\\(\"hsa\"\\)}, {fr\\(\"<unknown>\"\\)}, {fr\\(\"<unknown>\"\\)}\\), target: obj2\\) init\\(target: obj3\\) init\\(prefer_type\\({fr\\(\"hip\"\\)}, {fr\\(\"sycl\"\\)}, {fr\\(\"opencl\"\\)}\\), targetsync: obj4\\) init\\(prefer_type\\({fr\\(\"hip\"\\)}, {fr\\(\"sycl\"\\)}, {fr\\(\"opencl\"\\)}\\), targetsync: obj7\\) init\\(prefer_type\\({fr\\(\"level_zero\"\\)}, {fr\\(\"level_zero\"\\)}\\), target, targetsync: obj5\\)\[\r\n\]" 1 "original" } }


! --------------------------------------------

!$omp interop init ( target, prefer_type( {fr(1_"hip"), attr("ompx_gnu_prio:1", 1_"ompx_gnu_debug")}, {attr("ompx_gnu_nicest"), attr("ompx_something")}) : obj1, obj2) init ( prefer_type( {fr("cuda")}, {fr(omp_ifr_cuda_driver), attr("ompx_nix")}, {fr("best")}), targetsync : obj3, obj4) nowait use(obj5)    ! { dg-message "'#pragma omp interop' not yet supported" }
!
! ! { dg-warning "Unknown foreign runtime identifier 'best' at \\(1\\) \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
!
! { dg-final { scan-tree-dump-times "#pragma omp interop init\\(prefer_type\\({fr\\(\"hip\"\\),attr\\(\"ompx_gnu_prio:1\"\\),attr\\(\"ompx_gnu_debug\"\\)}, {attr\\(\"ompx_gnu_nicest\"\\),attr\\(\"ompx_something\"\\)}\\), target: obj1\\) init\\(prefer_type\\({fr\\(\"hip\"\\),attr\\(\"ompx_gnu_prio:1\"\\),attr\\(\"ompx_gnu_debug\"\\)}, {attr\\(\"ompx_gnu_nicest\"\\),attr\\(\"ompx_something\"\\)}\\), target: obj2\\) init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\),attr\\(\"ompx_nix\"\\)}, {fr\\(\"<unknown>\"\\)}\\), targetsync: obj3\\) init\\(prefer_type\\({fr\\(\"cuda\"\\)}, {fr\\(\"cuda_driver\"\\),attr\\(\"ompx_nix\"\\)}, {fr\\(\"<unknown>\"\\)}\\), targetsync: obj4\\) use\\(obj5\\) nowait\[\r\n\]" 1 "original" } }

end
