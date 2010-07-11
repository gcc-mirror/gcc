! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/44702
!
! Based on a test case by Joe Krahn.
!
! Multiple import of the same symbol was failing for
! intrinsic modules.
!
subroutine one()
  use iso_c_binding, only: a => c_ptr, b => c_ptr, c_ptr
  implicit none
  type(a) :: x
  type(b) :: y
  type(c_ptr) :: z
end subroutine one

subroutine two()
  use iso_c_binding, a => c_ptr, b => c_ptr
  implicit none
  type(a) :: x
  type(b) :: y
end subroutine two

subroutine three()
  use iso_fortran_env, only: a => error_unit, b => error_unit, error_unit
  implicit none
  if(a /= b) call shall_not_be_there()
  if(a /= error_unit) call shall_not_be_there()
end subroutine three

subroutine four()
  use iso_fortran_env, a => error_unit, b => error_unit
  implicit none
  if(a /= b) call shall_not_be_there()
end subroutine four

! { dg-final { scan-tree-dump-times "shall_not_be_there" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
