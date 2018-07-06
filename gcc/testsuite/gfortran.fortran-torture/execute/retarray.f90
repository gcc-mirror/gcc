! Program to test functions returning arrays

program testfnarray
   implicit none
   integer, dimension (6, 5) :: a
   integer n

! These first two shouldn't require a temporary.
   a = 0
   a = test(6, 5)
   if (a(1,1) .ne. 42) STOP 1
   if (a(6,5) .ne. 43) STOP 2

   a = 0
   a(1:6:2, 2:5) = test2()
   if (a(1,2) .ne. 42) STOP 3
   if (a(5,5) .ne. 43) STOP 4

   a = 1
   ! This requires a temporary
   a = test(6, 5) - a
   if (a(1,1) .ne. 41) STOP 5
   if (a(6,5) .ne. 42) STOP 6

   contains

   function test (x, y)
      implicit none
      integer x, y
      integer, dimension (1:x, 1:y) :: test

      test(1, 1) = 42
      test(x, y) = 43
   end function

   function test2 () result (foo)
      implicit none
      integer, dimension (3, 4) :: foo

      foo(1, 1) = 42
      foo(3, 4) = 43
   end function

end program

