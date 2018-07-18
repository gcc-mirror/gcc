! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! Program to test the UNPACK intrinsic for large real type
program intrinsic_unpack
   implicit none
   integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)

   real(kind=k), dimension(3,3) :: ark, brk
   complex(kind=k), dimension(3,3) :: ack, bck

   logical, dimension(3, 3) :: mask
   character(len=500) line1, line2
   integer i

   mask = reshape ((/.false.,.true.,.false.,.true.,.false.,.false.,&
                    &.false.,.false.,.true./), (/3, 3/));

   ark = reshape ((/1._k, 0._k, 0._k, 0._k, 1._k, 0._k, 0._k, 0._k, 1._k/), &
         (/3, 3/));
   brk = unpack ((/2._k, 3._k, 4._k/), mask, ark)
   if (any (brk .ne. reshape ((/1._k, 2._k, 0._k, 3._k, 1._k, 0._k, &
                               0._k, 0._k, 4._k/), (/3, 3/)))) &
      STOP 1
   write (line1,'(9F9.5)') brk
   write (line2,'(9F9.5)') unpack((/2._k, 3._k, 4._k/), mask, ark)
   if (line1 .ne. line2) STOP 2
   brk = -1._k
   brk = unpack ((/2._k, 3._k, 4._k/), mask, 0._k)
   if (any (brk .ne. reshape ((/0._k, 2._k, 0._k, 3._k, 0._k, 0._k, &
      0._k, 0._k, 4._k/), (/3, 3/)))) &
      STOP 3

   ack = reshape ((/1._k, 0._k, 0._k, 0._k, 1._k, 0._k, 0._k, 0._k, 1._k/), &
        (/3, 3/));
   bck = unpack ((/(2._k, 0._k), (3._k, 0._k), (4._k,   0._k)/), mask, ack)
   if (any (real(bck) .ne. reshape ((/1._k, 2._k, 0._k, 3._k, 1._k, 0._k, &
        0._k, 0._k, 4._k/), (/3, 3/)))) &
        STOP 4
   write (line1,'(18F9.5)') bck
   write (line2,'(18F9.5)') unpack((/(2._k, 0._k), (3._k, 0._k), (4._k,0._k)/), &
        mask, ack)
   if (line1 .ne. line2) STOP 5

end program
