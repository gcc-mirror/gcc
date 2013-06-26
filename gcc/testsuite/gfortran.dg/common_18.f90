! { dg-do compile }
!
! PR fortran/48858
!
!
use iso_c_binding
contains
subroutine one()
  bind(C, name="com1") :: /foo/
  integer(c_int) :: a
  common /foo/ a
end subroutine
subroutine two()
  integer(c_long) :: a
  common /foo/ a
end subroutine two
end

! { dg-final { scan-assembler "com1" } }
! { dg-final { scan-assembler "foo_" } }
