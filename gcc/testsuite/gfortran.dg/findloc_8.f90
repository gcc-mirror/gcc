! PR libfortran/95390
! { dg-do run { target fortran_real_10 } }

  complex(kind=10) :: a(6), b, d(2,2)
  logical :: m(6), n, o(2,2)
  integer :: c(1), e(2)
  a = (/ 1., 2., 17., 2., 2., 6. /)
  b = 17.
  c = findloc (a, b)
  if (c(1) /= 3) stop 1
  m = (/ .true., .false., .true., .true., .true., .true. /)
  n = .true.
  b = 2.
  c = findloc (a, b, m)
  if (c(1) /= 4) stop 2
  c = findloc (a, b, n)
  if (c(1) /= 2) stop 3
  d = reshape((/ 1., 2., 2., 3. /), (/ 2, 2 /))
  e = findloc (d, b, 1)
  if (e(1) /= 2 .or. e(2) /= 1) stop 4
  o = reshape((/ .true., .false., .true., .true. /), (/ 2, 2 /))
  e = findloc (d, b, 1, o)
  if (e(1) /= 0 .or. e(2) /= 1) stop 5
  e = findloc (d, b, 1, n)
  if (e(1) /= 2 .or. e(2) /= 1) stop 6
  n = .false.
  e = findloc (d, b, 1, n)
  if (e(1) /= 0 .or. e(2) /= 0) stop 7
end
