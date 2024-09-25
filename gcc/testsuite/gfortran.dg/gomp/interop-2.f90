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

program main
use m
implicit none

!$omp requires reverse_offload

integer(omp_interop_kind) :: obj1, obj2, obj3, obj4, obj5
integer :: x

!$omp interop init ( prefer_type( {fr(1_"") }) : obj1) ! { dg-error "Expected scalar integer parameter or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( {fr(1_"hip") , attr(omp_ifr_cuda) }) : obj1) ! { dg-error "Expected default-kind character literal" }

!$omp interop init ( prefer_type( {fr(1_"hip") , attr("myooption") }) : obj1) ! { dg-error "Character literal at .1. must start with 'ompx_'" }
!$omp interop init ( prefer_type( {fr(1_"hip") , attr("ompx_option") , attr("ompx_") } ) : obj1)
!$omp interop init ( prefer_type( {fr(1_"hip") , attr("ompx_option") }, { attr("ompx_") } ) : obj1)
!$omp interop init ( prefer_type( {fr(1_"hip") , attr("ompx_option") }  { attr("ompx_") } ) : obj1) ! { dg-error "Expected ',' or '\\)'" }
!$omp interop init ( prefer_type( {fr(1_"hip") , attr("ompx_option")   ) : obj1) ! { dg-error "Expected ',' or '\}'" }

!$omp interop init ( prefer_type( {fr(1_"hip") attr("ompx_option")   ) : obj1) ! { dg-error "Expected ',' or '\}'" }
!$omp interop init ( prefer_type( {fr(1_"hip")}), prefer_type("cuda") : obj1) ! { dg-error "Duplicate 'prefer_type' modifier" }

!$omp interop init ( prefer_type( {attr("ompx_option1,ompx_option2")   ) : obj1) ! { dg-error "Unexpected null or ',' character in character literal" }

!$omp interop init ( targetsync other ) : obj1)  ! { dg-error "Expected ',' or ':'" }
!$omp interop init ( prefer_type( {fr(1_"cuda") } ), other : obj1)  ! { dg-error "Expected 'target' or 'targetsync'" }
!$omp interop init ( prefer_type( {fr(1_"cuda") } ), obj1)  ! { dg-error "Expected 'target' or 'targetsync'" }
end
