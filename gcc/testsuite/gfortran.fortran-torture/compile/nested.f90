! Program to test the nested functions
program intrinsic_pack
   integer, parameter :: val(9) = (/0,0,0,0,9,0,0,0,7/)
   integer, dimension(3, 3) :: a
   integer, dimension(6) :: b

   a = reshape (val, (/3, 3/))
   b = 0
   b(1:6:3) = pack (a, a .ne. 0);
   if (any (b(1:6:3) .ne. (/9, 7/))) call abort
   b = pack (a(2:3, 2:3), a(2:3, 2:3) .ne. 0, (/1, 2, 3, 4, 5, 6/));
   if (any (b .ne. (/9, 7, 3, 4, 5, 6/))) call abort

contains
  subroutine tests_with_temp
    ! A few tests which involve a temporary
    if (any (pack(a, a.ne.0) .ne. (/9, 7/))) call abort
    if (any (pack(a, .true.) .ne. val)) call abort
    if (size(pack (a, .false.)) .ne. 0) call abort
    if (any (pack(a, .false., (/1,2,3/)).ne. (/1,2,3/))) call abort

  end subroutine tests_with_temp
end program
