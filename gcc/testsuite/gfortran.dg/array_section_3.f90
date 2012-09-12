! { dg-do compile }
!
! PR fortran/54225
!
! Contributed by robb wu
!
program test
  implicit none
  real :: A(2,3)

  print *, A(1, *)  ! { dg-error "Expected array subscript" }
end program

subroutine test2
integer, dimension(2) :: a
a(*) = 1  ! { dg-error "Expected array subscript" }
end
