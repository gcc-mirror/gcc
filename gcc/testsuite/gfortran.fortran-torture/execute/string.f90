! Program to test string handling
program string
   implicit none
   character(len=5) :: a, b
   character(len=20) :: c

   a = 'Hello'
   b = 'World'
   c = a//b

   if (c .ne. 'HelloWorld') STOP 1
   if (c .eq. 'WorldHello') STOP 2
   if (a//'World' .ne. 'HelloWorld') STOP 3
   if (a .ge. b) STOP 4
end program
