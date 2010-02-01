! { dg-do compile }
!
! PR fortran/42922
!
! Contributed by mrestelli@gmail.com
!
pure subroutine psub()
  implicit none
  type ilist
    integer :: i = 0
  end type ilist
  type(ilist) :: x
  x%i = 1
end subroutine psub
