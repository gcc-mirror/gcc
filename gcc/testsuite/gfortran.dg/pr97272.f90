! { dg-do run }
! PR fortran/97272 - Wrong answer from MAXLOC with character arg

program test
  implicit none
  integer :: i, j, k, l = 10
  character, allocatable :: a(:)
  allocate (a(l))
  a(:) = 'a'
  l = l - 1
  a(l) = 'b'
  i = maxloc (a, dim=1)
  j = maxloc (a, dim=1, kind=2)
  k = maxloc (a, dim=1, kind=8, back=.true.)
! print *, 'i = ', i, 'a(i) = ', a(i)
! print *, 'j = ', j, 'a(j) = ', a(j)
! print *, 'k = ', k, 'a(k) = ', a(k)
  if (i /= l .or. j /= l .or. k /= l) stop 1
end
