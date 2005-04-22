! { dg-do run }
! PR 20436: This used to give a runtime error.
program nested_reshape
  implicit none
  real :: k(8,2)
  real :: o(8,2)

  k = reshape((/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0, &
            9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0/), (/8,2/))

  o = reshape(reshape(k, (/2,8/), order=(/2,1/)), (/8,2/))
end program
