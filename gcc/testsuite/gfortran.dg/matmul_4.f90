! { dg-do run }
! Check the fix for PR28947, in which the mechanism for dealing
! with matmul (a, transpose (b)) would cause wrong results for
! a having a rank == 1.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>
!   
program gfcbug40
  implicit none

  real :: h(3,3), mat(2,3)

  h(:,:) = - HUGE (1.0)/4       ! Preset unused elements suitably...

  h(3,:) = 0
  h(3,3) = 1
  mat(:,:) = 1
  h(3,:) = h(3,:) + matmul (matmul (h(3,:), transpose (mat)), mat)

  if (any (h(3,:) .ne. (/2.0, 2.0, 3.0/))) STOP 1

end program gfcbug40
