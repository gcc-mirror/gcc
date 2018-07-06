! { dg-do run }
! PR 16946
! Not all allowed combinations of arguments for MAXVAL, MINVAL,
! PRODUCT and SUM were supported.
program reduction_mask
  implicit none
  logical :: equal(3)
  
  integer, parameter :: res(4*9) = (/ 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, &
       1, 1, 1, 1, 1, 6, 6, 6, 2, 2, 2, 2, 2, 2, 6, 6, 6, 3, 3, 3, 3, 3, 3 /)
  integer :: val(4*9)
  complex :: cval(2*9), cin(3)
  
  equal = (/ .true., .true., .false. /)
  
  ! use all combinations of the dim and mask arguments for the
  ! reduction intrinsics
  val( 1) = maxval((/ 1, 2, 3 /))
  val( 2) = maxval((/ 1, 2, 3 /), 1)
  val( 3) = maxval((/ 1, 2, 3 /), dim=1)
  val( 4) = maxval((/ 1, 2, 3 /), equal)
  val( 5) = maxval((/ 1, 2, 3 /), mask=equal)
  val( 6) = maxval((/ 1, 2, 3 /), 1, equal)
  val( 7) = maxval((/ 1, 2, 3 /), 1, mask=equal)
  val( 8) = maxval((/ 1, 2, 3 /), dim=1, mask=equal)
  val( 9) = maxval((/ 1, 2, 3 /), mask=equal, dim=1)
       
  val(10) = minval((/ 1, 2, 3 /))
  val(11) = minval((/ 1, 2, 3 /), 1)
  val(12) = minval((/ 1, 2, 3 /), dim=1)
  val(13) = minval((/ 1, 2, 3 /), equal)
  val(14) = minval((/ 1, 2, 3 /), mask=equal)
  val(15) = minval((/ 1, 2, 3 /), 1, equal)
  val(16) = minval((/ 1, 2, 3 /), 1, mask=equal)
  val(17) = minval((/ 1, 2, 3 /), dim=1, mask=equal)
  val(18) = minval((/ 1, 2, 3 /), mask=equal, dim=1)
       
  val(19) = product((/ 1, 2, 3 /))
  val(20) = product((/ 1, 2, 3 /), 1)
  val(21) = product((/ 1, 2, 3 /), dim=1)
  val(22) = product((/ 1, 2, 3 /), equal)
  val(23) = product((/ 1, 2, 3 /), mask=equal)
  val(24) = product((/ 1, 2, 3 /), 1, equal)
  val(25) = product((/ 1, 2, 3 /), 1, mask=equal)
  val(26) = product((/ 1, 2, 3 /), dim=1, mask=equal)
  val(27) = product((/ 1, 2, 3 /), mask=equal, dim=1)
       
  val(28) = sum((/ 1, 2, 3 /))
  val(29) = sum((/ 1, 2, 3 /), 1)
  val(30) = sum((/ 1, 2, 3 /), dim=1)
  val(31) = sum((/ 1, 2, 3 /), equal)
  val(32) = sum((/ 1, 2, 3 /), mask=equal)
  val(33) = sum((/ 1, 2, 3 /), 1, equal)
  val(34) = sum((/ 1, 2, 3 /), 1, mask=equal)
  val(35) = sum((/ 1, 2, 3 /), dim=1, mask=equal)
  val(36) = sum((/ 1, 2, 3 /), mask=equal, dim=1)
  
  if (any (val /= res)) STOP 1

  ! Tests for complex arguments. These were broken by the original fix.

  cin = cmplx((/1,2,3/))

  cval(1) = product(cin)
  cval(2) = product(cin, 1)
  cval(3) = product(cin, dim=1)
  cval(4) = product(cin, equal)
  cval(5) = product(cin, mask=equal)
  cval(6) = product(cin, 1, equal)
  cval(7) = product(cin, 1, mask=equal)
  cval(8) = product(cin, dim=1, mask=equal)
  cval(9) = product(cin, mask=equal, dim=1)
       
  cval(10) = sum(cin)
  cval(11) = sum(cin, 1)
  cval(12) = sum(cin, dim=1)
  cval(13) = sum(cin, equal)
  cval(14) = sum(cin, mask=equal)
  cval(15) = sum(cin, 1, equal)
  cval(16) = sum(cin, 1, mask=equal)
  cval(17) = sum(cin, dim=1, mask=equal)
  cval(18) = sum(cin, mask=equal, dim=1)

  if (any (cval /= cmplx(res(19:36)))) STOP 2
end program reduction_mask
