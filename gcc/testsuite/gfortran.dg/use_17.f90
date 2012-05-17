! { dg-do compile }
!
! PR fortran/51578
!
! Contributed by Billy Backer
!
! Check that indict importing of the symbol "axx" works
! even if renaming prevent the direct import.
!
module mod1
integer :: axx=2
end module mod1

module mod2
use mod1
end module mod2

subroutine sub1
use mod1, oxx=>axx
use mod2
implicit none
print*,axx ! Valid - was working before
end subroutine sub1

subroutine sub2
use mod2
use mod1, oxx=>axx
implicit none
print*,axx ! Valid - was failing before
end subroutine sub2

subroutine test1
  use :: iso_c_binding
  use, intrinsic :: iso_c_binding, only: c_double_orig => c_double
  integer :: c_double
  integer, parameter :: p1 = c_int, p2 = c_double_orig
end subroutine test1
