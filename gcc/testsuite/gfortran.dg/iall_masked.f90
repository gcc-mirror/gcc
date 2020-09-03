! { dg-do run }
! PR fortran/96890 - Wrong answer with intrinsic IALL
program p
  implicit none
  integer :: iarr1(0), iarr2(2,2), iarr3(2,2,2)
  logical :: mask1(0), mask2(2,2), mask3(2,2,2)

  if (     iall(iarr1,    mask1) /=          -1                 ) stop 1
  if (     iall(iarr1, 1, mask1) /=          -1                 ) stop 2

  iarr2 = reshape ([  1,      2,       3,      4      ], shape (iarr2))
  mask2 = reshape ([ .true., .false., .true., .false. ], shape (mask2))

  if (any (iall(iarr2, 2, mask2) /=          [1,-1])            ) stop 3

  iarr3 = reshape ([  1,      2,       3,      4,     &
                      5,      6,       7,      8      ], shape (iarr3))
  mask3 = reshape ([ .true., .false., .true., .false.,&
                     .true., .false., .true., .false. ], shape (iarr3))

  if (any (iall(iarr3, 2, mask3) /= reshape ([1,-1,5,-1],[2,2]))) stop 4
end
