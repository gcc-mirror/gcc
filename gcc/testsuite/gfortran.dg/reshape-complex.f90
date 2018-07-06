! { dg-do run }
! PR 21127:  Reshape of complex didn't work.
! PR 21480:  Reshape of packed complex arrays didn't work either.
program main
  complex, dimension(8) :: b
  complex, dimension(2,2) :: a
  complex, dimension(2) :: c,d
  integer :: i
  b = (/(i,i=1,8)/)
  a = reshape(b(1:8:2),shape(a))
  if (a(1,1) /= (1.0, 0.0) .or. a(2,1) /= (3.0, 0.0) .or.  &
      a(1,2) /= (5.0, 0.0) .or. a(2,2) /= (7.0, 0.0)) STOP 1
  c = (/( 3.14, -3.14), (2.71, -2.71)/)
  d = reshape(c, shape (d))
  if (any (c .ne. d)) STOP 2
end
