! PR Fortran/71523
!
! { dg-do compile }
! { dg-options "-fdump-tree-original -finit-local-zero -fautomatic -fmax-stack-var-size=8" }
!
! Make sure that variables larger than max-stack-var-size which become
! static are not given automatic initializers on function entry.
!

function set(idx, val)
  implicit none
  integer, intent(in) :: idx, val
  integer set
  integer arr(100)

  set = arr(idx)
  arr(idx) = val
  return
end function

! There should be no automatic initializer for arr
! { dg-final { scan-tree-dump-times "arr = " 0 "original" } }
