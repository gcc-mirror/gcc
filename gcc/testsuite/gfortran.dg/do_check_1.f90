! { dg-do run }
! { dg-options "-fcheck=do" }
! { dg-shouldfail "DO check" }
!
! PR fortran/34656
! Run-time check for zero STEP
!
program test
  implicit none
  integer :: i,j
  j = 0
  do i = 1, 40, j
    print *, i
  end do
end program test
! { dg-output "Fortran runtime error: DO step value is zero" }
