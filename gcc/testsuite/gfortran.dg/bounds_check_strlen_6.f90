! { dg-do run }
! { dg-options "-fbounds-check" }

! PR fortran/37746
! Ensure that too long or matching string lengths don't trigger the runtime
! error for matching string lengths, if the dummy argument is neither
! POINTER nor ALLOCATABLE or assumed-shape.
! Also check that absent OPTIONAL arguments don't trigger the check.

MODULE m
CONTAINS

  SUBROUTINE test (str, opt)
    IMPLICIT NONE
    CHARACTER(len=5) :: str
    CHARACTER(len=5), OPTIONAL :: opt
  END SUBROUTINE test

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE
  CALL test ('abcde')  ! String length matches.
  CALL test ('abcdef') ! String too long, is ok.
END PROGRAM main
