! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! Test that nested array constructors are optimized.
program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15)
  real(kind=dp), dimension(2,2) :: a
  real(kind=dp) thirteen

  data a /2._dp,3._dp,5._dp,7._dp/
  thirteen = 13._dp
  if (abs (product([[11._dp, thirteen], a]) - 30030._dp) > 1e-8) STOP 1
end program main
! { dg-final { scan-tree-dump-times "while" 2 "original" } }
