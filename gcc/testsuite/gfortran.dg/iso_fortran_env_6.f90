! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Check for new F2008 integer constants, needed for
! coarray support (cf. PR fortran/18918)
!

USE iso_fortran_env
implicit none
integer(kind=ATOMIC_INT_KIND) :: atomic_int ! { dg-error "has no IMPLICIT type" }
logical(kind=ATOMIC_LOGICAL_KIND) :: atomic_bool ! { dg-error "has no IMPLICIT type" }

if (IOSTAT_INQUIRE_INTERNAL_UNIT <= 0) call abort() ! { dg-error "has no IMPLICIT type" }
print *,STAT_STOPPED_IMAGE ! { dg-error "has no IMPLICIT type" }
print *, STAT_LOCKED_OTHER_IMAGE ! { dg-error "has no IMPLICIT type" }
print *, STAT_LOCKED ! { dg-error "has no IMPLICIT type" }
print *, STAT_UNLOCKED ! { dg-error "has no IMPLICIT type" }
end

module m
USE iso_fortran_env, only: ATOMIC_INT_KIND ! { dg-error "is not in the selected standard" }
implicit none
end module m

module m2
USE iso_fortran_env, only: foo => STAT_UNLOCKED ! { dg-error "is not in the selected standard" }
implicit none
end module m2

module m3
USE iso_fortran_env, foo => IOSTAT_INQUIRE_INTERNAL_UNIT ! { dg-error "not found" }
implicit none
end module m3
