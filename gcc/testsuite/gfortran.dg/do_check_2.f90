! { dg-do run }
! { dg-options "-fcheck=do" }
! { dg-shouldfail "DO check" }
!
! PR fortran/34656
! Run-time check for modifing loop variables
!
program test
  implicit none
  integer :: i,j
  do i = 1, 10
    call modLoopVar(i)
  end do
contains
  subroutine modLoopVar(i)
    integer :: i
    i = i + 1
  end subroutine modLoopVar
end program test
! { dg-output "Fortran runtime error: Loop variable has been modified" }
