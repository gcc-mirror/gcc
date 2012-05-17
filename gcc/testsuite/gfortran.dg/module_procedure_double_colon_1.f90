! { dg-do compile }
!
! PR fortran/49265
! Contributed by Erik Toussaint
!
module m1
   implicit none
   interface foo
      module procedure::bar
      module procedure ::bar_none
      module procedure:: none_bar
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
