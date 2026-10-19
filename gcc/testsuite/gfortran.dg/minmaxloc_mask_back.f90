! { dg-do run }
!
! PR fortran/127846 - MINLOC/MAXLOC with MASK and BACK=.true.
!
! Contributed by Federico Perini.

program minloc_mask_back
  implicit none
  real(8) :: y(3,4)
  logical :: m(3,4)
  integer :: i, j, l
  l = 0
  do j = 1, 4; do i = 1, 3
     y(i,j) = real(mod(l*7,5)+1, 8); m(i,j) = mod(l,3) /= 1; l = l + 1
  end do; end do
  if (any (minloc(y, mask=m, back=.true.) /= [ 3, 2 ])) then
     do i = 1, 3
        print '(4f4.1,6x,4l2)', y(i,:), m(i,:)
     end do
     print '(a,2i3,a)', 'minloc(y, mask=m, back=.true.) =', &
          minloc(y, mask=m, back=.true.), '   expected  3  2 (m(2,4) is F)'
     stop 1
  end if
  if (any (maxloc(y, mask=m, back=.true.) /= [ 3, 1 ])) then
     do i = 1, 3
        print '(4f4.1,6x,4l2)', y(i,:), m(i,:)
     end do
     print '(a,2i3,a)', 'maxloc(y, mask=m, back=.true.) =', &
          maxloc(y, mask=m, back=.true.), '   expected  3  1 (m(2,3) is F)'
     stop 2
  end if
end program minloc_mask_back
