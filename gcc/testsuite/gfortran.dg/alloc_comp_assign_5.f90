! { dg-do run }
! { dg-options "-O2" }
! Tests the fix for PR29428, in which the assignment of
! a function result would result in the function being
! called twice, if it were not a result by reference,
! because of a spurious nullify in gfc_trans_scalar_assign.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program test
implicit none

  type A
    integer, allocatable :: j(:)
  end type A

  type(A):: x
  integer :: ctr = 0

  x = f()

  if (ctr /= 1) call abort ()

contains

  function f()
    type(A):: f
      ctr = ctr + 1
      f = A ((/1,2/))
  end function f

end program

