! Test for the MALLOC and FREE intrinsics
! If something is wrong with them, this test might segfault
! { dg-do run }
  integer j
  integer*8 i8

  do j = 1, 10000
    i8 = malloc (10 * j)
    call free (i8)
  end do
  end
