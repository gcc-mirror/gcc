! { dg-do run }
program foo
   implicit none
   integer(kind=1), dimension (10) :: i_1
   integer(kind=1), dimension (2, 3) :: a_1
   integer(kind=1), dimension (2, 2, 3) :: b_1
   integer(kind=2), dimension (10) :: i_2
   integer(kind=2), dimension (2, 3) :: a_2
   integer(kind=2), dimension (2, 2, 3) :: b_2
   integer(kind=4), dimension (10) :: i_4
   integer(kind=4), dimension (2, 3) :: a_4
   integer(kind=4), dimension (2, 2, 3) :: b_4
   integer(kind=8), dimension (10) :: i_8
   integer(kind=8), dimension (2, 3) :: a_8
   integer(kind=8), dimension (2, 2, 3) :: b_8
   real(kind=4), dimension (10) :: r_4
   real(kind=4), dimension (2, 3) :: ar_4
   real(kind=4), dimension (2, 2, 3) :: br_4
   real(kind=8), dimension (10) :: r_8
   real(kind=8), dimension (2, 3) :: ar_8
   real(kind=8), dimension (2, 2, 3) :: br_8
   complex(kind=4), dimension (10) :: c_4
   complex(kind=4), dimension (2, 3) :: ac_4
   complex(kind=4), dimension (2, 2, 3) :: bc_4
   complex(kind=8), dimension (10) :: c_8
   complex(kind=8), dimension (2, 3) :: ac_8
   complex(kind=8), dimension (2, 2, 3) :: bc_8
   type i4_t
      integer(kind=4) :: v
   end type i4_t
   type(i4_t), dimension (10) :: it_4
   type(i4_t), dimension (2, 3) :: at_4
   type(i4_t), dimension (2, 2, 3) :: bt_4
   type(i4_t) :: iv_4

   character (len=200) line1, line2, line3

   a_1 = reshape ((/1_1, 2_1, 3_1, 4_1, 5_1, 6_1/), (/2, 3/))
   b_1 = spread (a_1, 1, 2)
   if (any (b_1 .ne. reshape ((/1_1, 1_1, 2_1, 2_1, 3_1, 3_1, 4_1, 4_1, 5_1, 5_1, 6_1, 6_1/), &
                            (/2, 2, 3/)))) &
      STOP 1
   line1 = ' '
   write(line1, 9000) b_1
   line2 = ' '
   write(line2, 9000) spread (a_1, 1, 2)
   if (line1 /= line2) STOP 2
   line3 = ' '
   write(line3, 9000) spread (a_1, 1, 2) + 0_1
   if (line1 /= line3) STOP 3
   i_1 = spread(1_1,1,10)
   if (any(i_1 /= 1_1)) STOP 4

   a_2 = reshape ((/1_2, 2_2, 3_2, 4_2, 5_2, 6_2/), (/2, 3/))
   b_2 = spread (a_2, 1, 2)
   if (any (b_2 .ne. reshape ((/1_2, 1_2, 2_2, 2_2, 3_2, 3_2, 4_2, 4_2, 5_2, 5_2, 6_2, 6_2/), &
                            (/2, 2, 3/)))) &
      STOP 5
   line1 = ' '
   write(line1, 9000) b_2
   line2 = ' '
   write(line2, 9000) spread (a_2, 1, 2)
   if (line1 /= line2) STOP 6
   line3 = ' '
   write(line3, 9000) spread (a_2, 1, 2) + 0_2
   if (line1 /= line3) STOP 7
   i_2 = spread(1_2,1,10)
   if (any(i_2 /= 1_2)) STOP 8

   a_4 = reshape ((/1_4, 2_4, 3_4, 4_4, 5_4, 6_4/), (/2, 3/))
   b_4 = spread (a_4, 1, 2)
   if (any (b_4 .ne. reshape ((/1_4, 1_4, 2_4, 2_4, 3_4, 3_4, 4_4, 4_4, 5_4, 5_4, 6_4, 6_4/), &
                            (/2, 2, 3/)))) &
      STOP 9
   line1 = ' '
   write(line1, 9000) b_4
   line2 = ' '
   write(line2, 9000) spread (a_4, 1, 2)
   if (line1 /= line2) STOP 10
   line3 = ' '
   write(line3, 9000) spread (a_4, 1, 2) + 0_4
   if (line1 /= line3) STOP 11
   i_4 = spread(1_4,1,10)
   if (any(i_4 /= 1_4)) STOP 12

   a_8 = reshape ((/1_8, 2_8, 3_8, 4_8, 5_8, 6_8/), (/2, 3/))
   b_8 = spread (a_8, 1, 2)
   if (any (b_8 .ne. reshape ((/1_8, 1_8, 2_8, 2_8, 3_8, 3_8, 4_8, 4_8, 5_8, 5_8, 6_8, 6_8/), &
                            (/2, 2, 3/)))) &
      STOP 13
   line1 = ' '
   write(line1, 9000) b_8
   line2 = ' '
   write(line2, 9000) spread (a_8, 1, 2)
   if (line1 /= line2) STOP 14
   line3 = ' '
   write(line3, 9000) spread (a_8, 1, 2) + 0_8
   if (line1 /= line3) STOP 15
   i_8 = spread(1_8,1,10)
   if (any(i_8 /= 1_8)) STOP 16


   ar_4 = reshape ((/1._4, 2._4, 3._4, 4._4, 5._4, 6._4/), (/2, 3/))
   br_4 = spread (ar_4, 1, 2)
   if (any (br_4 .ne. reshape ((/1._4, 1._4, 2._4, 2._4, 3._4, 3._4, &
   & 4._4, 4._4, 5._4, 5._4, 6._4, 6._4/), (/2, 2, 3/)))) STOP 17
   line1 = ' '
   write(line1, 9010) br_4
   line2 = ' '
   write(line2, 9010) spread (ar_4, 1, 2)
   if (line1 /= line2) STOP 18
   line3 = ' '
   write(line3, 9010) spread (ar_4, 1, 2) + 0._4
   if (line1 /= line3) STOP 19
   r_4 = spread(1._4,1,10)
   if (any(r_4 /= 1._4)) STOP 20


   ar_8 = reshape ((/1._8, 2._8, 3._8, 4._8, 5._8, 6._8/), (/2, 3/))
   br_8 = spread (ar_8, 1, 2)
   if (any (br_8 .ne. reshape ((/1._8, 1._8, 2._8, 2._8, 3._8, 3._8, &
   & 4._8, 4._8, 5._8, 5._8, 6._8, 6._8/), (/2, 2, 3/)))) STOP 21
   line1 = ' '
   write(line1, 9010) br_8
   line2 = ' '
   write(line2, 9010) spread (ar_8, 1, 2)
   if (line1 /= line2) STOP 22
   line3 = ' '
   write(line3, 9010) spread (ar_8, 1, 2) + 0._8
   if (line1 /= line3) STOP 23
   r_8 = spread(1._8,1,10)
   if (any(r_8 /= 1._8)) STOP 24

   ac_4 = reshape ((/(1._4,-1._4), (2._4,-2._4), (3._4, -3._4), (4._4, -4._4), &
                   & (5._4,-5._4), (6._4,-6._4)/), (/2, 3/))
   bc_4 = spread (ac_4, 1, 2)
   if (any (real(bc_4) .ne. reshape ((/1._4, 1._4, 2._4, 2._4, 3._4, 3._4, &
   & 4._4, 4._4, 5._4, 5._4, 6._4, 6._4/), (/2, 2, 3/)))) STOP 25
   if (any (-aimag(bc_4) .ne. reshape ((/1._4, 1._4, 2._4, 2._4, 3._4, 3._4, &
   & 4._4, 4._4, 5._4, 5._4, 6._4, 6._4/), (/2, 2, 3/)))) STOP 26
   line1 = ' '
   write(line1, 9020) bc_4
   line2 = ' '
   write(line2, 9020) spread (ac_4, 1, 2)
   if (line1 /= line2) STOP 27
   line3 = ' '
   write(line3, 9020) spread (ac_4, 1, 2) + 0._4
   if (line1 /= line3) STOP 28
   c_4 = spread((1._4,-1._4),1,10)
   if (any(c_4 /= (1._4,-1._4))) STOP 29

   ac_8 = reshape ((/(1._8,-1._8), (2._8,-2._8), (3._8, -3._8), (4._8, -4._8), &
                   & (5._8,-5._8), (6._8,-6._8)/), (/2, 3/))
   bc_8 = spread (ac_8, 1, 2)
   if (any (real(bc_8) .ne. reshape ((/1._8, 1._8, 2._8, 2._8, 3._8, 3._8, &
   & 4._8, 4._8, 5._8, 5._8, 6._8, 6._8/), (/2, 2, 3/)))) STOP 30
   if (any (-aimag(bc_8) .ne. reshape ((/1._8, 1._8, 2._8, 2._8, 3._8, 3._8, &
   & 4._8, 4._8, 5._8, 5._8, 6._8, 6._8/), (/2, 2, 3/)))) STOP 31
   line1 = ' '
   write(line1, 9020) bc_8
   line2 = ' '
   write(line2, 9020) spread (ac_8, 1, 2)
   if (line1 /= line2) STOP 32
   line3 = ' '
   write(line3, 9020) spread (ac_8, 1, 2) + 0._8
   if (line1 /= line3) STOP 33
   c_8 = spread((1._8,-1._8),1,10)
   if (any(c_8 /= (1._8,-1._8))) STOP 34


   at_4%v = reshape ((/1_4, 2_4, 3_4, 4_4, 5_4, 6_4/), (/2, 3/))
   bt_4 = spread (at_4, 1, 2)
   if (any (bt_4%v .ne. reshape ((/1_4, 1_4, 2_4, 2_4, 3_4, 3_4, 4_4, &
        & 4_4, 5_4, 5_4, 6_4, 6_4/), (/2, 2, 3/)))) &
      STOP 35
   iv_4%v = 123_4
   it_4 = spread(iv_4,1,10)
   if (any(it_4%v /= 123_4)) STOP 36


9000 format(12I3)
9010 format(12F7.3)
9020 format(25F7.3)

end program
