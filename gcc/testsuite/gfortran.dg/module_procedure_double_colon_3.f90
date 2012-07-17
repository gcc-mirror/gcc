! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/49265
!
! Contributed by Erik Toussaint
!
module m1
  implicit none
  interface foo
     procedure :: bar ! { dg-error "Fortran 2008: double colon in MODULE PROCEDURE statement" }
  end interface
contains
  subroutine bar
  end subroutine
end module
