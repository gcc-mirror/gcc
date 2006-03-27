! { dg-do run }
program main
  implicit none
  real, dimension(2) :: a
  a(1) = 2.0
  a(2) = 3.0
  if (product (a, .false.) /= 1.0) call abort
  if (product (a, .true.) /= 6.0) call abort
  if (sum (a, .false.) /= 0.0) call abort
  if (sum (a, .true.) /= 5.0) call abort
  if (maxval (a, .true.) /= 3.0) call abort
  if (maxval (a, .false.) > -1e38) call abort
  if (maxloc (a, 1, .true.) /= 2) call abort
  if (maxloc (a, 1, .false.) /= 0) call abort ! Change to F2003 requirement.
end program main
