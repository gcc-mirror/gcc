! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! PR 53148 - this used to cause wrong code because the label was
! placed after the statement assigning the new variables.
program main
  integer :: n
  double precision x
  n = 3
  goto 100
100 x = dble(n) + dble(n)
  if (x /= 6.d0) call abort
end program main
