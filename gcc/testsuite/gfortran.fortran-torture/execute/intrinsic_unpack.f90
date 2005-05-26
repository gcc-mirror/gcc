! Program to test the UNPACK intrinsic
program intrinsic_unpack
   integer, dimension(3, 3) :: a, b
   logical, dimension(3, 3) :: mask;
   character(len=50) line1, line2
   integer i

   mask = reshape ((/.false.,.true.,.false.,.true.,.false.,.false.,&
                    &.false.,.false.,.true./), (/3, 3/));
   a = reshape ((/1, 0, 0, 0, 1, 0, 0, 0, 1/), (/3, 3/));
   b = unpack ((/2, 3, 4/), mask, a)
   if (any (b .ne. reshape ((/1, 2, 0, 3, 1, 0, 0, 0, 4/), (/3, 3/)))) &
      call abort
   write (line1,'(10I4)') b
   write (line2,'(10I4)') unpack((/2, 3, 4/), mask, a)
   if (line1 .ne. line2) call abort
   b = -1
   b = unpack ((/2, 3, 4/), mask, 0)
   if (any (b .ne. reshape ((/0, 2, 0, 3, 0, 0, 0, 0, 4/), (/3, 3/)))) &
      call abort
end program
