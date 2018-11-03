! { dg-do run }
! Test different code paths for findloc with scalar result.

program main
  integer, dimension(0:5) :: a = [1,2,3,1,2,3]
  logical, dimension(6) :: mask = [.false.,.false.,.false.,.true.,.true.,.true.]
  logical, dimension(6) :: mask2
  logical :: true, false
  character(len=2), dimension(6) :: ch = ["AA", "BB", "CC", "AA", "BB", "CC"]

  true = .true.
  false = .false.
  mask2 = .not. mask

! Tests without mask

  if (findloc(a,2,dim=1,back=false) /= 2) stop 1
  if (findloc(a,2,dim=1,back=.false.) /= 2) stop 2
  if (findloc(a,2,dim=1) /= 2) stop 3
  if (findloc(a,2,dim=1,back=.true.) /= 5) stop 4
  if (findloc(a,2,dim=1,back=true) /= 5) stop 5

! Test with array mask
  if (findloc(a,2,dim=1,mask=mask) /= 5) stop 6
  if (findloc(a,2,dim=1,mask=mask,back=.true.) /= 5) stop 7
  if (findloc(a,2,dim=1,mask=mask,back=.false.) /= 5) stop 8
  if (findloc(a,2,dim=1,mask=mask2) /= 2) stop 9
  if (findloc(a,2,dim=1,mask=mask2,back=.true.) /= 2) stop 10
  if (findloc(a,2,dim=1,mask=mask2,back=true) /= 2) stop 11

! Test with scalar mask

  if (findloc(a,2,dim=1,mask=.true.) /= 2) stop 12
  if (findloc(a,2,dim=1,mask=.false.) /= 0) stop 13
  if (findloc(a,2,dim=1,mask=true) /= 2) stop 14
  if (findloc(a,2,dim=1,mask=false) /= 0) stop 15

! Some character tests

  if (findloc(ch,"AA",dim=1) /= 1) stop 16
  if (findloc(ch,"AA",dim=1,mask=mask) /= 4) stop 17
  if (findloc(ch,"AA",dim=1,back=.true.) /= 4) stop 18
  if (findloc(ch,"AA",dim=1,mask=mask2,back=.true.) /= 1) stop 19

! Nothing to be found here...
  if (findloc(ch,"DD",dim=1) /= 0) stop 20
  if (findloc(a,4,dim=1) /= 0) stop 21

! Finally, character tests with a scalar mask.

  if (findloc(ch,"CC ",dim=1,mask=true) /= 3) stop 22
  if (findloc(ch,"CC ",dim=1,mask=false) /= 0) stop 23
end program main
