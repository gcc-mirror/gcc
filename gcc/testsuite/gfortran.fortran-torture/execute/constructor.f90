! Program to test array constructors
program constructors
   integer, dimension (4) :: a
   integer, dimension (3, 2) :: b
   integer i, j, k, l, m, n

   a = (/1, (i,i=2,4)/)
   do i = 1, 4
      if (a(i) .ne. i) call abort
   end do

   b = reshape ((/0, 1, 2, 3, 4, 5/), (/3, 2/)) + 1
   do i=1,3
      if (b(i, 1) .ne. i) call abort
      if (b(i, 2) .ne. i + 3) call abort
   end do

   k = 1
   l = 2
   m = 3
   n = 4
   ! The remainder assumes constant constructors work ok.
   a = (/n, m, l, k/)
   if (any (a .ne. (/4, 3, 2, 1/))) call abort
   a = (/((/i+10, 42/), i = k, l)/)
   if (any (a .ne. (/11, 42, 12, 42/))) call abort
   a = (/(I, I=k,l) , (J, J=m,n)/)
   if (any (a .ne. (/1, 2, 3, 4/))) call abort
end program
