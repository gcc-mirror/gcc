! { dg-do run }
! Test the fix for PR31879 in which the concatenation operators below
! would cause ICEs because the character lengths were never resolved.
!
! Contributed by Vivek Rao <vivekrao4@yahoo.com> 
!
module str_mod
  character(3) :: mz(2) = (/"fgh","ijk"/)
contains
  function ccopy(yy) result(xy)
    character (len=*), intent(in) :: yy(:)
    character (len=5) :: xy(size(yy))
    xy = yy
  end function ccopy
end module str_mod
!
program xx
  use str_mod, only: ccopy, mz
  implicit none
  character(2) :: z = "zz"
  character(3) :: zz(2) = (/"abc","cde"/)
  character(2) :: ans(2)
  integer :: i = 2, j = 3
  if (any(ccopy("_&_"//(/"A","B"/)//"?") .ne. (/"_&_A?","_&_B?"/))) STOP 1
  if (any(ccopy(z//zz) .ne. (/"zzabc","zzcde"/))) STOP 2
  if (any(ccopy(z//zz(:)(1:2)) .ne. (/"zzab ","zzcd "/))) STOP 3
  if (any(ccopy(z//mz(:)(2:3)) .ne. (/"zzgh ","zzjk "/))) STOP 4

! This was another bug, uncovered when the PR was fixed.
  if (any(ccopy(z//mz(:)(i:j)) .ne. (/"zzgh ","zzjk "/))) STOP 5
end program xx
