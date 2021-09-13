! { dg-do compile { target { fortran_integer_16 || ilp32 } } }
! omp_depend_kind = 2*intptr_t --> 16 (128 bit) on 64bit-pointer systems
!                              --> 8  (128 bit) on 32bit-pointer systems
subroutine f1
  !use omp_lib   ! N/A in gcc/testsuite
  use iso_c_binding, only: c_intptr_t
  implicit none
  integer, parameter :: omp_depend_kind = 2*c_intptr_t
  integer :: a
  integer(kind=omp_depend_kind) :: depobj1, depobj2, depobj3, depobj4, depobj5
  !$omp depobj(depobj1) depend (in : a)
  !$omp depobj(depobj2) depend (out : a)
  !$omp depobj(depobj3) depend( inout : a)
  !$omp depobj(depobj4) depend(mutexinoutset: a)
  !$omp depobj(depobj1) update(out)
  !$omp depobj(depobj2) update(mutexinoutset)
  !$omp depobj(depobj3) update(in)
  !$omp depobj(depobj4) update(inout)
  !$omp task depend (depobj: depobj1, depobj2, depobj3)
  !$omp end task

  !$omp task depend(mutexinoutset: a)
  !$omp end task
  !$omp depobj(depobj2) destroy
end subroutine f1
