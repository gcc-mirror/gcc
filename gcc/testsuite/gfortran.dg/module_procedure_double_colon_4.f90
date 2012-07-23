! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/49265
!
! Contributed by Erik Toussaint
!
module m1
  implicit none
  interface foo
     procedure :: bar ! "::" is valid since Fortran 2008
  end interface
contains
  subroutine bar
  end subroutine
end module
