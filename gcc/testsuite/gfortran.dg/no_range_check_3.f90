! { dg-do run }
program test
  integer(2) :: j, k
  integer :: i
  i = int(z'FFFFFFFF',kind(i))
  if (i /= -1) STOP 1
  if (int(z'FFFFFFFF',kind(i)) /= -1) STOP 2

  if (popcnt(int(z'0F00F00080000001',8)) /= 10) STOP 3
  if (popcnt(int(z'800F0001',4)) /= 6) STOP 4

  j = -1234_2
  k = int(z'FB2E',kind(j))
  if (k /= j) STOP 5
  if (int(z'FB2E',kind(j)) /= j) STOP 6
end program test
