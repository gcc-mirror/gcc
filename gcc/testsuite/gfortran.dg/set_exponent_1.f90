! { dg-do run }
! PR fortran/109511
! Check compile-time simplification of SET_EXPONENT against runtime

program exponent
  implicit none
  integer :: i
  i = 0
  print *, i, set_exponent(1., 0), set_exponent(1., i)
  if (set_exponent(1., 0) /= set_exponent(1., i)) stop 1
  i = 1
  print *, i, set_exponent(1., 1), set_exponent(1., i)
  if (set_exponent(1., 1) /= set_exponent(1., i)) stop 2
  i = 2
  print *, i, set_exponent(-1.75, 2), set_exponent(-1.75, i)
  if (set_exponent(-1.75, 2) /= set_exponent(-1.75, i)) stop 3
  print *, i, set_exponent(0.1875, 2), set_exponent(0.1875, i)
  if (set_exponent(0.1875, 2) /= set_exponent(0.1875, i)) stop 4
  i = 3
  print *, i, set_exponent(0.75, 3), set_exponent(0.75, i)
  if (set_exponent(0.75, 3) /= set_exponent(0.75, i)) stop 5
  i = 4
  print *, i, set_exponent(-2.5, 4), set_exponent(-2.5, i)
  if (set_exponent(-2.5, 4) /= set_exponent(-2.5, i)) stop 6
  i = -1
  print *, i, set_exponent(1., -1), set_exponent(1., i)
  if (set_exponent(1., -1) /= set_exponent(1., i)) stop 7
  i = -2
  print *, i, set_exponent(1.125, -2), set_exponent(1.125, i)
  if (set_exponent(1.125, -2) /= set_exponent(1.125, i)) stop 8
  print *, i, set_exponent(-0.25, -2), set_exponent(-0.25, i)
  if (set_exponent(-0.25, -2) /= set_exponent(-0.25, i)) stop 9
  i = -3
  print *, i, set_exponent(0.75, -3), set_exponent(0.75, i)
  if (set_exponent(0.75, -3) /= set_exponent(0.75, i)) stop 10
end program exponent
