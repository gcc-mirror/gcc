! Program to test the PACK intrinsic
program intrinsic_pack
   integer, parameter :: val(9) = (/0,0,0,0,9,0,0,0,7/)
   integer, dimension(3, 3) :: a
   integer, dimension(6) :: b

   a = reshape (val, (/3, 3/))
   b = 0
   b(1:6:3) = pack (a, a .ne. 0);
   if (any (b(1:6:3) .ne. (/9, 7/))) STOP 1
   b = pack (a(2:3, 2:3), a(2:3, 2:3) .ne. 0, (/1, 2, 3, 4, 5, 6/));
   if (any (b .ne. (/9, 7, 3, 4, 5, 6/))) STOP 2

   call tests_with_temp()
contains
  subroutine tests_with_temp
    ! A few tests which involve a temporary
    if (any (pack(a, a.ne.0) .ne. (/9, 7/))) STOP 3
    if (any (pack(a, .true.) .ne. val)) STOP 4
    if (size(pack (a, .false.)) .ne. 0) STOP 5
    if (any (pack(a, .false., (/1,2,3/)).ne. (/1,2,3/))) STOP 6

  end subroutine tests_with_temp
end program
