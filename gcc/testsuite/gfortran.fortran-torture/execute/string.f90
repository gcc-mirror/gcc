! Program to test string handling
program string
   implicit none
   character(len=5) :: a, b
   character(len=20) :: c

   a = 'Hello'
   b = 'World'
   c = a//b

   if (c .ne. 'HelloWorld') call abort
   if (c .eq. 'WorldHello') call abort
   if (a//'World' .ne. 'HelloWorld') call abort
   if (a .ge. b) call abort
end program
