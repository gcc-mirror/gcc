! { dg-do compile }
! { dg-options "-Wextra" }
      program main
      integer, parameter :: x=3 ! { dg-warning "Unused parameter" }
      real :: a
      read (*,*) a
      if (a .eq. 3.14) a=2.     ! { dg-warning "Equality comparison" }
      print *,a
      end
