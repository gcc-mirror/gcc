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

   if (any (a .ne. 1)) call abort
   if (test(11) .ne. 42) call abort
   ! The second call should return
   if (test(0) .ne. 11) call abort

   if (c .ne. "Hello World") call abort
   if (d .ne. "Teststring") call abort
end program
