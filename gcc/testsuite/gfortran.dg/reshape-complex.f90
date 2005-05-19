! { dg-do run }
! PR 21127:  Reshape of complex didn't work.
program main
  complex, dimension(8) :: b
  complex, dimension(2,2) :: a
  integer :: i
  b = (/(i,i=1,8)/)
  a = reshape(b(1:8:2),shape(a))
  if (a(1,1) /= (1.0, 0.0) .or. a(2,1) /= (3.0, 0.0) .or.  &
      a(1,2) /= (5.0, 0.0) .or. a(2,2) /= (7.0, 0.0)) call abort
end
