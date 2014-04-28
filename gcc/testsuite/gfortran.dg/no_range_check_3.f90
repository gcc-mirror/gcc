! { dg-do run }
! { dg-options "-fno-range-check" }
program test
  integer :: i
  i = int(z'FFFFFFFF',kind(i))
  if (i /= -1) call abort
  if (int(z'FFFFFFFF',kind(i)) /= -1) call abort

  if (popcnt(int(z'0F00F00080000001',8)) /= 10) call abort
  if (popcnt(int(z'800F0001',4)) /= 6) call abort

end program test
