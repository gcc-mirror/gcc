! { dg-do compile }
! PR69397
program p
   interface f
      procedure f1 ! { dg-error "neither function nor subroutine" }
      !... more
   end interface
   integer, allocatable :: z
   print *, f(z) ! { dg-error "no specific function" }
contains
   integer function f2 (x)
      integer, allocatable :: x
      f2 = 1
   end
end

