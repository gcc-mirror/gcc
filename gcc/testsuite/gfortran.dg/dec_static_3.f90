! { dg-do compile }
! { dg-options "" }
!
! Check errors for use of STATIC/AUTOMATIC without -fdec-static.
!

subroutine s()
  implicit none
  integer, automatic :: a ! { dg-error "is a DEC extension" }
  integer, static :: b ! { dg-error "is a DEC extension" }
  integer, save :: c

  integer :: auto1, auto2, static1, static2, save1, save2
  automatic auto1 ! { dg-error "is a DEC extension" }
  automatic :: auto2 ! { dg-error "is a DEC extension" }
  static static1 ! { dg-error "is a DEC extension" }
  static :: static2 ! { dg-error "is a DEC extension" }
  save save1
  save :: save2
end subroutine
