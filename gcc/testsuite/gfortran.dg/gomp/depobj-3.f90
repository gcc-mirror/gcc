! { dg-do compile { target { fortran_integer_16 || ilp32 } } }
! omp_depend_kind = 2*intptr_t --> 16 (128 bit) on 64bit-pointer systems
!                              --> 8  (128 bit) on 32bit-pointer systems
subroutine f1
  !use omp_lib   ! N/A in gcc/testsuite
  use iso_c_binding, only: c_intptr_t
  implicit none
  integer, parameter :: omp_depend_kind = 2*c_intptr_t
  integer :: a, b
  integer(kind=omp_depend_kind) :: depobj, depobj1(5), depobj2

  !$omp depobj(depobj) destroy

  !$omp depobj(depobj) destroy( depobj)

  !$omp depobj(depobj) destroy( depobj2)  ! { dg-warning "The same depend object should be used as DEPOBJ argument at .1. and as DESTROY argument at .2." }
  !$omp depobj(depobj) destroy( a)  ! { dg-warning "The same depend object should be used as DEPOBJ argument at .1. and as DESTROY argument at .2." }
end
