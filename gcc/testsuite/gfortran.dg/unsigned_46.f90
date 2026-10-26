! { dg-do run }
! { dg-additional-options "-funsigned" }

! Check for implementation of leadz and trailz for unsigned types.
program memain
  integer(kind=1) :: i1
  integer(kind=2) :: i2
  integer(kind=4) :: i4
  integer(kind=8) :: i8
  unsigned(kind=1) :: u1
  unsigned(kind=2) :: u2
  unsigned(kind=4) :: u4
  unsigned(kind=8) :: u8

  i1 = 12_1
  u1 = 12u_1
  if (leadz(i1) /= leadz(u1)) stop 1
  if (leadz(u1) /= leadz(12u_1)) stop 1
  if (trailz(i1) /= trailz(u1)) stop 3
  if (trailz(u1) /= trailz(12u_1)) stop 3
  if (popcnt(i1) /= popcnt(u1)) stop 5
  if (popcnt(u1) /= popcnt(12u_1)) stop 5
  if (poppar(i1) /= poppar(u1)) stop 7
  if (poppar(u1) /= poppar(12u_1)) stop 7
  i2 = 1348_2
  u2 = 1348u_2
  if (leadz(i2) /= leadz(u2)) stop 9
  if (leadz(u2) /= leadz(1348u_2)) stop 9
  if (trailz(i2) /= trailz(u2)) stop 11
  if (trailz(u2) /= trailz(1348u_2)) stop 11
  if (popcnt(i2) /= popcnt(u2)) stop 13
  if (popcnt(u2) /= popcnt(1348u_2)) stop 13
  if (poppar(i2) /= poppar(u2)) stop 15
  if (poppar(u2) /= poppar(1348u_2)) stop 15
  i4 = 172472_4
  u4 = 172472u_4
  if (leadz(i4) /= leadz(u4)) stop 17
  if (leadz(u4) /= leadz(172472u_4)) stop 17
  if (trailz(i4) /= trailz(u4)) stop 19
  if (trailz(u4) /= trailz(172472u_4)) stop 19
  if (popcnt(i4) /= popcnt(u4)) stop 21
  if (popcnt(u4) /= popcnt(172472u_4)) stop 21
  if (poppar(i4) /= poppar(u4)) stop 23
  if (poppar(u4) /= poppar(172472u_4)) stop 23
  i8 = 18744625769701376_8
  u8 = 18744625769701376u_8
  if (leadz(i8) /= leadz(u8)) stop 25
  if (leadz(u8) /= leadz(18744625769701376u_8)) stop 25
  if (trailz(i8) /= trailz(u8)) stop 27
  if (trailz(u8) /= trailz(18744625769701376u_8)) stop 27
  if (popcnt(i8) /= popcnt(u8)) stop 29
  if (popcnt(u8) /= popcnt(18744625769701376u_8)) stop 29
  if (poppar(i8) /= poppar(u8)) stop 31
  if (poppar(u8) /= poppar(18744625769701376u_8)) stop 31

end program memain
