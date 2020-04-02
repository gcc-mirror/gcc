! { dg-do compile }
! { dg-options "-fdec-structure -fdec-static" }
!
! PR fortran/85982
!
! Test a regression wherein some component attributes were erroneously accepted
! within a DEC structure.
!

structure /s/
  integer :: a
  integer, intent(in) :: b ! { dg-error "is not allowed" }
  integer, intent(out) :: c ! { dg-error "is not allowed" }
  integer, intent(inout) :: d ! { dg-error "is not allowed" }
  integer, dimension(1,1) :: e ! OK
  integer, external, pointer :: f ! { dg-error "is not allowed" }
  integer, intrinsic :: f ! { dg-error "is not allowed" }
  integer, optional :: g ! { dg-error "is not allowed" }
  integer, parameter :: h ! { dg-error "is not allowed" }
  integer, protected :: i ! { dg-error "is not allowed" }
  integer, private :: j ! { dg-error "is not allowed" }
  integer, static :: k ! { dg-error "is not allowed" }
  integer, automatic :: l ! { dg-error "is not allowed" }
  integer, public :: m ! { dg-error "is not allowed" }
  integer, save :: n ! { dg-error "is not allowed" }
  integer, target :: o ! { dg-error "is not allowed" }
  integer, value :: p ! { dg-error "is not allowed" }
  integer, volatile :: q ! { dg-error "is not allowed" }
  integer, bind(c) :: r ! { dg-error "is not allowed" }
  integer, asynchronous :: t ! { dg-error "is not allowed" }
  character(len=3) :: v ! OK
  integer(kind=4) :: w ! OK
end structure

end
