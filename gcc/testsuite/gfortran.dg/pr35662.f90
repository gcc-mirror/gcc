! PR target/35662
! { dg-do run }
! { dg-options "-O1" }

subroutine f(x, y, z)
  real, intent (in) :: x
  real, intent (out) :: y, z
  y = sin (x)
  z = cos (x)
end subroutine f

program pr35662
  real :: x, y, z
  x = 3.1415926535897932384626433832795029
  call f (x, y, z)
  if (abs (y) > 1.0e-5 .or. abs (z + 1.0) > 1.0e-5) STOP 1
  x = x / 2.0
  call f (x, y, z)
  if (abs (y - 1.0) > 1.0e-5 .or. abs (z) > 1.0e-5) STOP 2
end program pr35662
