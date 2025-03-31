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

integer(omp_interop_kind) :: obj1, obj2, obj3, obj4, obj5
integer(omp_interop_kind) :: target, targetsync,prefer_type
integer :: x

!$omp interop init(target: obj1) init(target,targetsync : obj2, obj3) nowait

!$omp interop init(prefer_type(1_"cuda", omp_ifr_opencl, omp_ifr_level_zero, "hsa"), targetsync : obj1) &
!$omp&        destroy(obj2, obj3) depend(inout: x) use(obj4, obj5) device(device_num: 0)

!$omp assume contains(interop)
  !$omp interop init(target, prefer_type("cu da") : obj3)  ! { dg-warning "Unknown foreign runtime identifier 'cu da'" }
!$omp end assume

!$omp interop init(target: obj1, obj2, obj1), use(obj4) destroy(obj4)
! { dg-error "Symbol 'obj1' present on multiple clauses" "" { target *-*-* } .-1 }
! { dg-error "Symbol 'obj4' present on multiple clauses" "" { target *-*-* } .-2 }

!$omp interop depend(inout: x)  ! { dg-error "DEPEND clause at .1. requires action clause with 'targetsync' interop-type" }

!$omp interop depend(inout: x) , use(obj2), destroy(obj3) !  OK, use or destroy might have 'targetsync'

!$omp interop depend(inout: x) use(obj2), destroy(obj3) !  Likewise

!$omp interop depend(inout: x) use(obj2), destroy(obj3) init(target: obj4) ! { dg-error "DEPEND clause at .1. requires 'targetsync' interop-type, lacking it for 'obj4' at .2." }

!$omp interop depend(inout: x) init(targetsync : obj5)  use(obj2), destroy(obj3) init(target: obj4) ! { dg-error "DEPEND clause at .1. requires 'targetsync' interop-type, lacking it for 'obj4' at .2." } 
!$omp interop depend(inout: x) init(targetsync : obj5)  use(obj2), destroy(obj3) init(prefer_type("cuda"), targetsync : obj4) ! OK

!$omp interop init(target, targetsync, prefer_type, obj1) ! { dg-error "51: Expected '\\(' after 'prefer_type'" }
!$omp interop init(target, prefer_type, obj1, targetsync) ! { dg-error "39: Expected '\\(' after 'prefer_type'" }

! Duplicated variable name or duplicated modifier:
!$omp interop init(target, targetsync,target : obj1)  ! { dg-error "Duplicate 'target' at \\(1\\)" }
!$omp interop init(target, targetsync,target)         ! { dg-error "Duplicate 'target' at \\(1\\)" }
!$omp interop init(target : target, targetsync,target)  ! { dg-error "Symbol 'target' present on multiple clauses" }

!$omp interop init(target, targetsync,targetsync : obj1)  ! { dg-error "Duplicate 'targetsync' at \\(1\\)" }
!$omp interop init(target, targetsync,targetsync)         ! { dg-error "Duplicate 'targetsync' at \\(1\\)" }
!$omp interop init(target : target, targetsync,targetsync)  ! { dg-error "Symbol 'targetsync' present on multiple clauses" }


!$omp interop init(, targetsync, prefer_type, obj1, target)  ! { dg-error "20: Expected 'prefer_type', 'target', or 'targetsync'" }
end
