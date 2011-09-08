! { dg-do run }
! PR 50192 - on little-endian systems, this used to fail.
program main
  character(kind=4,len=2) :: c1, c2
  c1 = 4_' '
  c2 = 4_' '
  c1(1:1) = transfer(257, mold=c1(1:1))
  c2(1:1) = transfer(64, mold=c2(1:1))
  if (c1 < c2) call abort
end program main
