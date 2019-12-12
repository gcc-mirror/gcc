! { dg-do run }
! { dg-options "-std=gnu -w" }
! { dg-additional-sources lrshift_1.c }
program test_rshift_lshift
  implicit none
  integer :: i(15), j, n
  integer, external :: c_lshift, c_rshift

  i = (/ -huge(i), -huge(i)/2, -129, -128, -127, -2, -1, 0, &
         1, 2, 127, 128, 129, huge(i)/2, huge(i) /)

  do n = 1, size(i)
    do j = 0, 31
      if (lshift(i(n),j) /= c_lshift(i(n),j)) STOP 1
      if (rshift(i(n),j) /= c_rshift(i(n),j)) STOP 2
    end do
  end do
end program test_rshift_lshift
