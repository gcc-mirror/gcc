! Program to test static variable initialization
! returns the parameter from the previous invocation, or 42 on the first call.
function test (parm)
   implicit none
   integer test, parm
   integer :: val = 42

   test = val
   val = parm
end function

program intializer
   implicit none
   integer test
   character(11) :: c = "Hello World"
   character(15) :: d = "Teststring"
   integer, dimension(3) :: a = 1

   if (any (a .ne. 1)) STOP 1
   if (test(11) .ne. 42) STOP 2
   ! The second call should return
   if (test(0) .ne. 11) STOP 3

   if (c .ne. "Hello World") STOP 4
   if (d .ne. "Teststring") STOP 5
end program
