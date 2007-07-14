! Test for the MVBITS subroutine
! This used to fail on big-endian architectures (PR 32357)
! { dg-do run }
  integer(kind=8) :: i8 = 0
  integer(kind=4) :: i4 = 0
  integer(kind=2) :: i2 = 0
  integer(kind=1) :: i1 = 0
  call mvbits (1_1, 0, 8, i1, 0)
  if (i1 /= 1) call abort
  call mvbits (1_2, 0, 16, i2, 0)
  if (i2 /= 1) call abort
  call mvbits (1_4, 0, 16, i4, 0)
  if (i4 /= 1) call abort
  call mvbits (1_8, 0, 16, i8, 0)
  if (i8 /= 1) call abort
  end
