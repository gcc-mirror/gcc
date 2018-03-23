! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR fortran/33626
! Types were not always propagated correctly
  logical(kind=1) :: i, j
  integer(kind=1) :: a, b
  character*1 :: c, d
  if (any( (/ kind(i .and. j), kind(.not. (i .and. j)), kind((a + b)), &
              kind((42_1)), kind((j .and. i)), kind((.true._1)), &
              kind(c // d), kind((c) // d), kind((c//d)) /) /= 1 )) STOP 1
  if (any( (/ len(c // d), len((c) // d), len ((c // d)) /) /= 2)) STOP 2
end
