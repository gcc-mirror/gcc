! { dg-do run }
! { dg-require-effective-target fortran_large_int }

  integer(kind=16) :: i16

  i16 = -1
  if (leadz(i16) /= 0) call abort
  if (trailz(i16) /= 0) call abort
  if (leadz(-1_16) /= 0) call abort
  if (trailz(-1_16) /= 0) call abort

  i16 = -64
  if (leadz(i16) /= 0) call abort
  if (trailz(i16) /= 6) call abort
  if (leadz(-64_16) /= 0) call abort
  if (trailz(-64_16) /= 6) call abort

  i16 = -108
  if (leadz(i16) /= 0) call abort
  if (trailz(i16) /= 2) call abort
  if (leadz(-108_16) /= 0) call abort
  if (trailz(-108_16) /= 2) call abort

  i16 = 1
  if (leadz(i16) /= bit_size(i16) - 1) call abort
  if (trailz(i16) /= 0) call abort
  if (leadz(1_16) /= bit_size(1_16) - 1) call abort
  if (trailz(1_16) /= 0) call abort

  i16 = 64
  if (leadz(i16) /= 121) call abort
  if (trailz(i16) /= 6) call abort
  if (leadz(64_16) /= 121) call abort
  if (trailz(64_16) /= 6) call abort

end
