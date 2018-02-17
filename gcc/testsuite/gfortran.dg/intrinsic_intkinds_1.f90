! { dg-do run }
! Test assorted intrinsics for integer kinds 1 and 2
program main
  integer(kind=1), dimension(2,2) :: a
  integer(kind=2), dimension(2,2) :: b
  integer(kind=1), dimension(2) :: r1
  integer(kind=2), dimension(2) :: r2
  logical, dimension(2,2) :: ma
  ma = .false.
  a = reshape((/ 1_1, 2_1, 3_1, 4_1/), shape(a))
  b = reshape((/ 1_2, 2_2, 3_2, 4_2/), shape(b))
  if (any(sum(a,dim=2) /= (/ 4, 6 /))) STOP 1
  if (any(sum(b,dim=2) /= (/ 4, 6 /))) STOP 2
  if (any(product(a,dim=2) /= (/ 3, 8 /))) STOP 3
  if (any(product(b,dim=2) /= (/ 3, 8 /))) STOP 4
  if (any(matmul(a,a) /= reshape ( (/ 7, 10, 15, 22 /), shape(a)))) STOP 5
  if (any(matmul(b,b) /= reshape ( (/ 7, 10, 15, 22 /), shape(b)))) STOP 6
  if (any(maxval(a,dim=2,mask=ma) /= -128)) STOP 7
  if (any(maxval(b,dim=2,mask=ma) /= -32768)) STOP 8
end program main
