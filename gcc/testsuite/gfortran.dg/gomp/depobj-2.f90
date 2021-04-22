! { dg-do compile { target { fortran_integer_16 || ilp32 } } }
! omp_depend_kind = 2*intptr_t --> 16 (128 bit) on 64bit-pointer systems
!                              --> 8  (128 bit) on 32bit-pointer systems
subroutine f1
  !use omp_lib   ! N/A in gcc/testsuite
  use iso_c_binding, only: c_intptr_t
  implicit none
  integer, parameter :: omp_depend_kind = 2*c_intptr_t
  integer :: a, b
  integer(kind=omp_depend_kind) :: depobj, depobj1(5)
  real :: r
  integer(1) :: d
  
  !$omp depobj                                       ! { dg-error "Expected '\\( depobj \\)\'" }
  !$omp depobj(depobj)                               ! { dg-error "Expected DEPEND, UPDATE, or DESTROY clause" }
  !$omp depobj destroy                               ! { dg-error "Expected '\\( depobj \\)\'" }
  !$omp depobj ( depobj1 ( 1 ) ) depend( inout : a)  ! OK
  !$omp depobj(depobj1) depend( inout : a)           ! { dg-error "DEPOBJ in DEPOBJ construct at .1. shall be a scalar integer of OMP_DEPEND_KIND kind" }
  !$omp depobj(depobj1(:)) depend( inout : a)        ! { dg-error "DEPOBJ in DEPOBJ construct at .1. shall be a scalar integer of OMP_DEPEND_KIND kind" }
  !$omp depobj(r) depend( inout : a)                 ! { dg-error "DEPOBJ in DEPOBJ construct at .1. shall be a scalar integer of OMP_DEPEND_KIND kind" }
  !$omp depobj(d) depend( inout : a)                 ! { dg-error "DEPOBJ in DEPOBJ construct at .1. shall be a scalar integer of OMP_DEPEND_KIND kind" }
  !$omp depobj(depobj) depend( inout : a, b)         ! { dg-error "DEPEND clause at .1. of OMP DEPOBJ construct shall have only a single locator" }
  !$omp depobj(depobj) depend(mutexinoutset : a)     ! OK
  !$omp depobj(depobj) depend(source)                ! { dg-error "DEPEND clause at .1. of OMP DEPOBJ construct shall not have dependence-type SOURCE, SINK or DEPOBJ" }
  !$omp depobj(depobj) depend(sink : i + 1)          ! { dg-error "DEPEND clause at .1. of OMP DEPOBJ construct shall not have dependence-type SOURCE, SINK or DEPOBJ" }
  !$omp depobj(depobj) update(source)                ! { dg-error "Expected IN, OUT, INOUT, MUTEXINOUTSET followed by '\\)'" }
  !$omp depobj(depobj) update(sink)                  ! { dg-error "Expected IN, OUT, INOUT, MUTEXINOUTSET followed by '\\)'" }
  !$omp depobj(depobj) update(depobj)                ! { dg-error "Expected IN, OUT, INOUT, MUTEXINOUTSET followed by '\\)'" }

  ! Valid in OpenMP 5.1:
  !$omp depobj(depobj5) depend(depobj: depobj3)      ! { dg-error "DEPEND clause at .1. of OMP DEPOBJ construct shall not have dependence-type SOURCE, SINK or DEPOBJ" }
end subroutine f1

