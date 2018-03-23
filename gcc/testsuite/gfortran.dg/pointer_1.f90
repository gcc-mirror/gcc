! Testcase for PR34770
! { dg-do run }
  implicit none
  integer, target :: x(0:12)
  integer, pointer :: z(:)
  integer i
  do i = 0,12
    x(i) = i
  enddo
  z => x
  do i = 0,12
    if (x(i) /= i .or. z(i) /= i) STOP 1
  enddo
end
