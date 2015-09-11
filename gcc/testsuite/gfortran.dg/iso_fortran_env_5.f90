! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
!
! Check for new F2008 integer constants, needed for
! coarray support (cf. PR fortran/18918)
!

USE iso_fortran_env
implicit none
integer :: i
integer(kind=ATOMIC_INT_KIND) :: atomic_int
logical(kind=ATOMIC_LOGICAL_KIND) :: atomic_bool

i = 0
if (IOSTAT_INQUIRE_INTERNAL_UNIT <= 0) call abort()
if (IOSTAT_INQUIRE_INTERNAL_UNIT == STAT_STOPPED_IMAGE) call abort()
if (STAT_STOPPED_IMAGE <= 0) call abort()

if ((STAT_LOCKED_OTHER_IMAGE == STAT_LOCKED) &
    .or.(STAT_LOCKED_OTHER_IMAGE == STAT_UNLOCKED)) call abort()
if (STAT_LOCKED == STAT_UNLOCKED) call abort()

end

! { dg-final { scan-tree-dump-times "abort" 0 "original" } }

