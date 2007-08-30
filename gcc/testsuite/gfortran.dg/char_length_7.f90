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
  if (any(ccopy("_&_"//(/"A","B"/)//"?") .ne. (/"_&_A?","_&_B?"/))) call abort ()
  if (any(ccopy(z//zz) .ne. (/"zzabc","zzcde"/))) call abort ()
  if (any(ccopy(z//zz(:)(1:2)) .ne. (/"zzab ","zzcd "/))) call abort ()
  if (any(ccopy(z//mz(:)(2:3)) .ne. (/"zzgh ","zzjk "/))) call abort ()

! This was another bug, uncovered when the PR was fixed.
  if (any(ccopy(z//mz(:)(i:j)) .ne. (/"zzgh ","zzjk "/))) call abort ()
end program xx
! { dg-final { cleanup-modules "str_mod" } }
