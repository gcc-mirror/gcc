! { dg-do run }
!
! PR fortran/127845 - FINDLOC with DIM and MASK
!
! Contributed by Federico Perini.

program findloc_dim_mask
  implicit none
  real(8) :: x(2,3,2)
  logical :: m(2,3,2)
  integer :: i, j, k, l
  integer :: r(2,3)

  l = 0
  do k = 1, 2; do j = 1, 3; do i = 1, 2
     x(i,j,k) = real(mod(l*7,5)+1, 8); m(i,j,k) = mod(l,3) /= 1; l = l + 1
  end do; end do; end do
  r = findloc(x, 3.0d0, dim=3, mask=m)

  if (any (r /= transpose (reshape ([2, 0, 0, 0, 0, 2], [3,2])))) then
     do k = 1, 2
        print '(a,i0,a,i0,a)', 'x(:,:,', k, ')          m(:,:,', k, ')'
        do i = 1, 2
           print '(3f4.1,6x,3l2)', x(i,:,k), m(i,:,k)
        end do
     end do
     print '(a)', 'findloc(x, 3.0d0, dim=3, mask=m), a 2x3 array, got:'
     do i = 1, 2
        print '(4x,3i2)', r(i,:)
     end do
     print '(a)', '  expected:'
     print '(4x,3i2)', 2, 0, 0
     print '(4x,3i2)', 0, 0, 2
     print '(a)', '  (x(2,3,2) = 3.0 and m(2,3,2) = T, so element (2,3) is 2)'
     stop 1
  end if
end program findloc_dim_mask
