! { dg-do run }
! PR 34565 - intenal writes with negative strides.  This
! test case tries out a negative stride in a higher
! dimension.
program main
  implicit none
  integer :: i
  integer, parameter :: n1=2, n2=3, n3=5
  character(len=n1*n2*n3*2) :: line
  character(len=2), dimension(n1,n2,n3):: c
  write (unit=c(:,n2:1:-1,:),fmt="(I2)") (i,i=1,n1*n2*n3)
  line = transfer(c,mold=line)
  if (line /=" 5 6 3 4 1 21112 910 7 8171815161314232421221920293027282526") call abort
end program main
