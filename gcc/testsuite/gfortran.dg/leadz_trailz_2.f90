! { dg-do run }
! { dg-require-effective-target fortran_large_int }

  integer(kind=16) :: i16

  i16 = -1
  if (leadz(i16) /= 0) STOP 1
  if (trailz(i16) /= 0) STOP 2
  if (leadz(-1_16) /= 0) STOP 3
  if (trailz(-1_16) /= 0) STOP 4

  i16 = -64
  if (leadz(i16) /= 0) STOP 5
  if (trailz(i16) /= 6) STOP 6
  if (leadz(-64_16) /= 0) STOP 7
  if (trailz(-64_16) /= 6) STOP 8

  i16 = -108
  if (leadz(i16) /= 0) STOP 9
  if (trailz(i16) /= 2) STOP 10
  if (leadz(-108_16) /= 0) STOP 11
  if (trailz(-108_16) /= 2) STOP 12

  i16 = 1
  if (leadz(i16) /= bit_size(i16) - 1) STOP 13
  if (trailz(i16) /= 0) STOP 14
  if (leadz(1_16) /= bit_size(1_16) - 1) STOP 15
  if (trailz(1_16) /= 0) STOP 16

  i16 = 64
  if (leadz(i16) /= 121) STOP 17
  if (trailz(i16) /= 6) STOP 18
  if (leadz(64_16) /= 121) STOP 19
  if (trailz(64_16) /= 6) STOP 20

end
