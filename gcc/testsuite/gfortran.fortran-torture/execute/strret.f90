! Program to test caracter string return values
function test ()
   implicit none
   character(len=10) :: test
   test = "World"
end function

function test2 () result (r)
   implicit none
   character(len=5) :: r
   r = "Hello"
end function

program strret
   implicit none
   character(len=15) :: s
   character(len=10) :: test
   character(len=5) :: test2

   s = test ()
   if (s .ne. "World") call abort

   s = "Hello " // test ()
   if (s .ne. test2 () //" World") call abort
end
