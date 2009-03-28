! { dg-do run }
! { dg-options "-fcheck=do" }
! { dg-shouldfail "DO check" }
!
! PR fortran/34656
! Run-time check for modifing loop variables
!
program test
  implicit none
  real :: i, j, k
  j = 10.0
  k = 1.0
  do i = 1.0, j, k ! { dg-warning "must be integer" }
    call modLoopVar(i)
  end do
contains
  subroutine modLoopVar(x)
    real :: x
    x = x + 1
  end subroutine modLoopVar
end program test
! { dg-output "Fortran runtime error: Loop variable has been modified" }
