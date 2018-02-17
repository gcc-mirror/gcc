! { dg-do run }
program main
  implicit none
  real, dimension(2) :: a
  a(1) = 2.0
  a(2) = 3.0
  if (product (a, .false.) /= 1.0) STOP 1
  if (product (a, .true.) /= 6.0) STOP 2
  if (sum (a, .false.) /= 0.0) STOP 3
  if (sum (a, .true.) /= 5.0) STOP 4
  if (maxval (a, .true.) /= 3.0) STOP 5
  if (maxval (a, .false.) > -1e38) STOP 6
  if (maxloc (a, 1, .true.) /= 2) STOP 7
  if (maxloc (a, 1, .false.) /= 0) STOP 8! Change to F2003 requirement.
end program main
