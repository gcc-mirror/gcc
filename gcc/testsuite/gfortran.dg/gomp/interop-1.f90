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

subroutine sub1  ! { dg-error "Program unit at .1. has OpenMP device constructs/routines but does not set !.OMP REQUIRES REVERSE_OFFLOAD but other program units do" }
  !$omp interop
  integer :: y ! { dg-error "Unexpected data declaration statement" }
end subroutine sub1

program main
use m
implicit none

!$omp requires reverse_offload

integer(omp_interop_fr_kind), parameter :: ifr_array(2) = [omp_ifr_cuda, omp_ifr_hip]

integer(omp_interop_kind) :: obj1, obj2, obj3, obj4, obj5
integer :: x

!$omp interop init(obj1) init(target,targetsync : obj2, obj3) nowait ! OK
!$omp interop init(obj1) init (targetsync  : obj2, obj3) nowait ! OK
!$omp interop init(obj1) init (targetsync , target : obj2, obj3) nowait ! OK

!$omp interop init(obj1) init(target,targetsync,target: obj2, obj3) nowait ! { dg-error "Duplicate 'target'" }
!$omp interop init(obj1) init(target,targetsync, targetsync : obj2, obj3) nowait ! { dg-error "Duplicate 'targetsync'" }

!$omp interop init(prefer_type("cuda", omp_ifr_opencl, omp_ifr_level_zero, "hsa"), targetsync : obj1) &
!$omp&        destroy(obj2, obj3) depend(inout: x) use(obj4, obj5) device(device_num: 0)

!$omp interop init(prefer_type("cu" // "da"), targetsync : obj1) ! { dg-error "37: Expected ',' or '\\)'" }
! { dg-warning "Unknown foreign runtime identifier 'cu' at \\(1\\) \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }

!$omp assume contains(interop)
  !$omp interop init(prefer_type("cuða") : obj3)  ! { dg-warning "Unknown foreign runtime identifier 'cu\[^'\]*a'" }
!$omp end assume

!$omp interop init(prefer_type("cu"//char(0)//"da") : obj3) ! { dg-error "36: Expected ',' or '\\)'" }
! { dg-warning "Unknown foreign runtime identifier 'cu' at \\(1\\) \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }

!$omp interop depend(inout: x) , use(obj2), destroy(obj3) !  OK, use or destroy might have 'targetsync'

!$omp interop depend(inout: x) use(obj2), destroy(obj3) !  Likewise

!$omp interop depend(inout: x) init(targetsync : obj5)  use(obj2), destroy(obj3) init(prefer_type("cuda"), targetsync : obj4) ! OK

!$omp interop init ( target , prefer_type( { fr("hsa") }, "hip") : obj1) ! { dg-error "Expected '\{' at .1." }

!$omp interop init ( target , prefer_type( { fr("hsa"), attr("ompx_nothing") , fr("hsa" ) }) :obj1) ! { dg-error "Duplicated 'fr' preference-selector-name" }

!$omp interop init ( prefer_type( 4, omp_ifr_hip*4) : obj1)  ! { dg-warning "Unknown foreign runtime identifier '20'" }
!$omp interop init ( prefer_type( sin(3.3) : obj1)  ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( {fr(4 ) }) : obj1) ! OK
!$omp interop init ( prefer_type( {fr(4_"cuda" ) }) : obj1) ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( {fr(c_char_"cuda") }) : obj1) ! OK
!$omp interop init ( prefer_type( {fr(1_"cuda" ) }) : obj1) ! OK
!$omp interop init ( prefer_type( {fr(omp_ifr_level_zero ) }, {fr(omp_ifr_hip)}) : obj1) ! OK
!$omp interop init ( prefer_type( {fr("cuda" // "_driver") }) : obj1) ! { dg-error "46: Expected '\\)'" }
!$omp interop init ( prefer_type( {fr(trim("cuda" // "_driver")) }) : obj1) ! { dg-error "38: Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( {fr("hello" }) : obj1) ! { dg-error "47: Expected '\\)'" }
! { dg-warning "Unknown foreign runtime identifier 'hello' at \\(1\\) \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }

!$omp interop init ( prefer_type( {fr(x) }) : obj1) ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( {fr(ifr_array ) }) : obj1) ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( {fr(ifr_array(1) ) }) : obj1)

!$omp interop init ( prefer_type( omp_ifr_level_zero, omp_ifr_hip ) : obj1) ! OK
!$omp interop init ( prefer_type( omp_ifr_level_zero +1 ) : obj1) ! OK
!$omp interop init ( prefer_type( x ) : obj1) ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( ifr_array ) : obj1) ! { dg-error "Expected constant scalar integer expression or non-empty default-kind character literal" }
!$omp interop init ( prefer_type( ifr_array(2) ) : obj1) ! OK

!$omp interop init ( prefer_type( 4, omp_ifr_hip*4) : obj1) ! { dg-warning "Unknown foreign runtime identifier '20'" }
!$omp interop init ( prefer_type( 4, 1, 3) : obj1)

!$omp interop init ( prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }) : obj1)
!$omp interop init ( prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa,omp_ifr_level_zero)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }) : obj1) ! { dg-error "65: Expected '\\)'" }
!$omp interop init ( prefer_type( {fr("cuda",5) }, {fr(omp_ifr_hsa,omp_ifr_level_zero)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }) : obj1) ! { dg-error "45: Expected '\\)' at" }
!$omp interop init ( prefer_type( {fr("sycl"), attr("ompx_1", "ompx_2"), attr("ompx_3") }, {attr("ompx_4", "ompx_5"),fr(omp_ifr_level_zero)} ) : obj1)
!$omp interop init ( prefer_type( { fr(5), attr("ompx_1") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } ) : obj1)

end
