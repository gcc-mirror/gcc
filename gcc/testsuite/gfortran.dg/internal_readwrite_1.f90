! { dg-do run }
! PR 34565 - internal writes with negative strides
! didn't work.
program main
  implicit none
  integer :: i
  integer :: lo, up, st
  character(len=2) :: c (5)
  integer, dimension(5) :: n
  c = (/ 'a', 'b', 'c', 'd', 'e' /)
  write (unit=c(5:1:-2),fmt="(A)") '5','3', '1'
  write (unit=c(2:4:2),fmt="(A)") '2', '4'
  read  (c(5:1:-1),fmt="(I2)") (n(i), i=5,1,-1)
  if (any(n /= (/ (i,i=1,5) /))) call abort
end program main
