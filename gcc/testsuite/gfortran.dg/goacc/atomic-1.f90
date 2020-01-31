! { dg-do compile }
!
! PR fortran/93462
!
! Contributed by G. Steinmetz
!
program p
   integer :: n = 1
   integer :: a
!$acc atomic write
   a = f(n) - f(n)
contains
   integer function f(x)
      integer, intent(in) :: x
      f = x
   end
end
