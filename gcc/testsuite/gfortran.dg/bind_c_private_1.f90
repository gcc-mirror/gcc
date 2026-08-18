! { dg-do compile }
! { dg-require-visibility "" }
!
! PR fortran/126872
!
! A binding label gives external linkage, so PRIVATE must hide only the
! Fortran name and must not give the symbol hidden ELF visibility.
! Module entities without a binding label keep the hidden visibility
! introduced by PR fortran/125430.

module m
  use iso_c_binding
  implicit none
  private
  integer(c_int), bind(C, name="bc_var") :: bc_var
  integer :: plain_var
contains
  subroutine bc_named() bind(C, name="bc_named")
  end subroutine bc_named
  subroutine bc_unnamed() bind(C)
  end subroutine bc_unnamed
  subroutine plain_sub()
  end subroutine plain_sub
end module m

! { dg-final { scan-not-hidden "bc_var" } }
! { dg-final { scan-not-hidden "bc_named" } }
! { dg-final { scan-not-hidden "bc_unnamed" } }
! { dg-final { scan-hidden "__m_MOD_plain_var" } }
! { dg-final { scan-hidden "__m_MOD_plain_sub" } }
