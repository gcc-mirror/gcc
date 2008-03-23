! { dg-do run }
! { dg-require-effective-target fortran_large_int }
! Program to test the UNPACK intrinsic for a long integer type
program intrinsic_unpack
   implicit none
   integer,parameter :: k = selected_int_kind (range (0_8) + 1)
   integer(kind=k), dimension(3, 3) :: ak, bk
   logical, dimension(3, 3) :: mask
   character(len=100) line1, line2
   integer i

   mask = reshape ((/.false.,.true.,.false.,.true.,.false.,.false.,&
                    &.false.,.false.,.true./), (/3, 3/));

   ak = reshape ((/1, 0, 0, 0, 1, 0, 0, 0, 1/), (/3, 3/));
   bk = unpack ((/2_k, 3_k, 4_k/), mask, ak)
   if (any (bk .ne. reshape ((/1, 2, 0, 3, 1, 0, 0, 0, 4/), (/3, 3/)))) &
      call abort
   write (line1,'(10I4)') bk
   write (line2,'(10I4)') unpack((/2_k, 3_k, 4_k/), mask, ak)
   if (line1 .ne. line2) call abort
   bk = -1
   bk = unpack ((/2_k, 3_k, 4_k/), mask, 0_k)
   if (any (bk .ne. reshape ((/0, 2, 0, 3, 0, 0, 0, 0, 4/), (/3, 3/)))) &
      call abort

end program
