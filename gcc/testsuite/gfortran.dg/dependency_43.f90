! { dg-do run }
! { dg-options "-Warray-temporaries" }
! PR fortran/56937 - unnecessary temporaries with vector indices
program main
  integer, dimension(3) :: i1, i2
  real :: a(3,2)

  data a / 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 /
  i1 = [ 1, 2, 3 ]
  i2 = [ 3, 2, 1 ]
  a (i1,1) = a (i2,2)
  if (a(1,1) /= 6.0 .or. a(2,1) /= 5.0 .or. a(3,1) /= 4.0) call abort
  if (a(1,2) /= 4.0 .or. a(2,2) /= 5.0 .or. a(3,2) /= 6.0) call abort
end program main
