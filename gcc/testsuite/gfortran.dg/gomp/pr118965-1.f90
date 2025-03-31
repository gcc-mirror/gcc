! At least one of the target and/or targetsync modifiers must be provided.
! This implies that there are always modifiers required, and the parser
! should reject e.g. "init (var1, var2)"; the first thing in the list is
! always an init_modifier in valid code.

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
integer(omp_interop_kind) :: obj1, obj2

  !$omp interop init (obj1) ! { dg-error "Expected 'prefer_type', 'target', or 'targetsync'" }
  !$omp interop init (obj1, obj2) ! { dg-error "Expected 'prefer_type', 'target', or 'targetsync'" }
  !$omp interop init (obj1, target) ! { dg-error "Expected 'prefer_type', 'target', or 'targetsync'" }
  !$omp interop init (target, obj1) ! { dg-error "Expected 'prefer_type', 'target', or 'targetsync'" }
  !$omp interop init (obj1, targetsync) ! { dg-error "Expected 'prefer_type', 'target', or 'targetsync'" }
  !$omp interop init (targetsync, obj1) ! { dg-error "Expected 'prefer_type', 'target', or 'targetsync'" }
  !$omp interop init (targetsync, target) ! { dg-error "Expected ',' or ':'" }

  !$omp interop init (target, prefer_type( {fr(4 ) }) : obj1) ! OK
  !$omp interop init (targetsync, prefer_type( {fr(4 ) }) : obj1) ! OK
  !$omp interop init (prefer_type( {fr(4 ) }), target : obj1) ! OK

  !$omp interop init (prefer_type( {fr(4 ) }) : obj1) ! { dg-error "Missing required 'target' and/or 'targetsync' modifier" }

  ! This does not complain about foobar not being declared because
  ! Fortran parser error handling eats the whole rest of the statement.
  !$omp interop init (prefer_type( {fr(4 ) }) : foobar) ! { dg-error "Missing required 'target' and/or 'targetsync' modifier" }

end