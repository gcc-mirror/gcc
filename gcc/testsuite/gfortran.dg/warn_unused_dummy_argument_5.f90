! { dg-do compile }
! { dg-additional-options "-Wunused-dummy-argument" }
! PR 91557 - this used to generate a bogus warning
! Test case by Gerhard Steinmetz
program p
   integer :: a, b
   a = 1
   call g
contains
   subroutine g
      integer :: x, y
      call h (x, y)
      if ( a > 0 )   y = y - 1
      b = y - x + 1
   end
end
