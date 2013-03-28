! { dg-do compile }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
program main
  implicit none
  real :: a,b,c,d
  call random_number(a)
  call random_number(b)
  call random_number(c)
  call random_number(d)
  if (any ([a,b,c,d] < 0.2)) print *,"foo"
end program main
! { dg-final { scan-tree-dump-times "\\\|\\\|" 3 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
