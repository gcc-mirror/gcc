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

subroutine s(ointent)
use m
implicit none
integer(omp_interop_kind), parameter :: op = 0
integer(omp_interop_kind),intent(in) :: ointent
integer(omp_interop_kind) :: od(5)
integer(1) :: o1
integer, parameter :: mykind = mod (omp_interop_kind, 100) ! remove saving the 'comes from c_int' info
real(mykind) :: or

!$omp interop init (op)      ! { dg-error "'op' at \\(1\\) in 'INIT' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
                             ! { dg-error "Object 'op' is not a variable at \\(1\\)" "" { target *-*-* } .-1 }
!$omp interop init (ointent) ! { dg-error "'ointent' at \\(1\\) in 'INIT' clause must be definable" }
!$omp interop init (od)      ! { dg-error "'od' at \\(1\\) in 'INIT' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
!$omp interop init (od(1))   ! { dg-error "Syntax error in OpenMP variable list" }
!$omp interop init (o1)      ! { dg-error "'o1' at \\(1\\) in 'INIT' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
!$omp interop init (or)      ! { dg-error "'or' at \\(1\\) in 'INIT' clause must be a scalar integer variable of 'omp_interop_kind' kind" }

!$omp interop use (op)      ! { dg-error "'op' at \\(1\\) in 'USE' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
                            ! { dg-error "Object 'op' is not a variable at \\(1\\)" "" { target *-*-* } .-1 }
!$omp interop use (ointent) ! okay
!$omp interop use (od)      ! { dg-error "'od' at \\(1\\) in 'USE' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
!$omp interop use (od(1))   ! { dg-error "Syntax error in OpenMP variable list" }
!$omp interop use (o1)      ! { dg-error "'o1' at \\(1\\) in 'USE' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
!$omp interop use (or)      ! { dg-error "'or' at \\(1\\) in 'USE' clause must be a scalar integer variable of 'omp_interop_kind' kind" }

!$omp interop destroy (op)      ! { dg-error "'op' at \\(1\\) in 'DESTROY' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
                                ! { dg-error "Object 'op' is not a variable at \\(1\\)" "" { target *-*-* } .-1 }
!$omp interop destroy (ointent) ! { dg-error "'ointent' at \\(1\\) in 'DESTROY' clause must be definable" }
!$omp interop destroy (od)      ! { dg-error "'od' at \\(1\\) in 'DESTROY' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
!$omp interop destroy (od(1))   ! { dg-error "Syntax error in OpenMP variable list" }
!$omp interop destroy (o1)      ! { dg-error "'o1' at \\(1\\) in 'DESTROY' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
!$omp interop destroy (or)      ! { dg-error "'or' at \\(1\\) in 'DESTROY' clause must be a scalar integer variable of 'omp_interop_kind' kind" }

end subroutine

program main
use m
implicit none

integer(omp_interop_kind) :: obj1, obj2, obj3, obj4, obj5
integer :: x

!$omp interop init ( prefer_type( {fr(1_"") }) : obj1) ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
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
