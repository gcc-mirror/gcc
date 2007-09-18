! { dg-do compile }
!
! PR fortran/33231
!
! Elemental function:
! Intent OUT/INOUT dummy: Actual needs to be an array
! if any actual is an array
!
program prog
implicit none
integer :: i, j(2)
call sub(i,1,2) ! OK, only scalar
call sub(j,1,2) ! OK, scalar IN, array OUT
call sub(j,[1,2],3) ! OK, scalar & array IN, array OUT
call sub(j,[1,2],[1,2]) ! OK, all arrays

call sub(i,1,2) ! OK, only scalar
call sub(i,[1,2],3) ! { dg-error "is a scalar" }
call sub(i,[1,2],[1,2]) ! { dg-error "is a scalar" }
contains
elemental subroutine sub(a,b,c)
  integer :: func, a, b, c
  intent(in) :: b,c
  intent(out) :: a
  a = b +c
end subroutine sub
end program prog
