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
if (IOSTAT_INQUIRE_INTERNAL_UNIT <= 0) STOP 1
if (IOSTAT_INQUIRE_INTERNAL_UNIT == STAT_STOPPED_IMAGE) STOP 2
if (STAT_STOPPED_IMAGE <= 0) STOP 3

if ((STAT_LOCKED_OTHER_IMAGE == STAT_LOCKED) &
    .or.(STAT_LOCKED_OTHER_IMAGE == STAT_UNLOCKED)) STOP 4
if (STAT_LOCKED == STAT_UNLOCKED) STOP 5

end

! { dg-final { scan-tree-dump-times "_gfortran_stop" 0 "original" } }

