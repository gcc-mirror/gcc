! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/49265
! Contributed by Erik Toussaint
!
module m1
   implicit none
   interface foo
      module procedure::bar       ! { dg-error "double colon" }
      module procedure ::bar_none ! { dg-error "double colon" }
      module procedure:: none_bar ! { dg-error "double colon" }
   end interface
contains
   subroutine bar
   end subroutine
   subroutine bar_none(i)
     integer i
   end subroutine
   subroutine none_bar(x)
     real x
   end subroutine
end module
