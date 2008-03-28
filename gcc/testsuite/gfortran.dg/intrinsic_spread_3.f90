! { dg-do run }
! { dg-require-effective-target fortran_large_int }

program foo
   implicit none
   integer,parameter :: k = selected_int_kind (range (0_8) + 1)

   integer(kind=k), dimension(10) :: i_k
   integer(kind=k), dimension (2, 3) :: a_k
   integer(kind=k), dimension (2, 2, 3) :: b_k
   character (len=200) line1, line2, line3

   a_k = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k/), (/2, 3/))
   b_k = spread (a_k, 1, 2)
   if (any (b_k .ne. reshape ((/1_k, 1_k, 2_k, 2_k, 3_k, 3_k, 4_k, 4_k, 5_k, 5_k, 6_k, 6_k/), &
                            (/2, 2, 3/)))) &
      call abort
   line1 = ' '
   write(line1, 9000) b_k
   line2 = ' '
   write(line2, 9000) spread (a_k, 1, 2)
   if (line1 /= line2) call abort
   line3 = ' '
   write(line3, 9000) spread (a_k, 1, 2) + 0_k
   if (line1 /= line3) call abort
   i_k = spread(1_k,1,10)
   if (any(i_k /= 1_k)) call abort

9000 format(12I3)

end program
