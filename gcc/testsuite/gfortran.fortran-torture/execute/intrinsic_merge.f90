! Program to test the MERGE intrinsic
program intrinsic_merge
   integer, dimension(3) :: a, b
   integer i

   a = (/-1, 2, 3/)

   i = 5
   if (merge (-1, 1, i .gt. 3) .ne. -1) call abort
   i = 1
   if (merge (-1, 1, i .ge. 3) .ne. 1) call abort

   b = merge(a, 0, a .ge. 0)
   if (any (b .ne. (/0, 2, 3/))) call abort
end program
