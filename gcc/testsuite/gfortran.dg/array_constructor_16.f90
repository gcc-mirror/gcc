! { dg-do run }
! Tests the fix for PR31204, in which 'i' below would be incorrectly
! host associated by the contained subroutines.  The checks for 'ii'
! and 'iii' have been added, since they can be host associated because
! of the explicit declarations in the main program.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  integer ii
  INTEGER, PARAMETER :: jmin(1:10) = (/ (i, i = 1, 10) /)
  INTEGER, PARAMETER :: kmin(1:10) = (/ (ii, ii = 1, 10) /)
  INTEGER, PARAMETER :: lmin(1:10) = (/ (iii, iii = 1, 10) /)
  integer iii
  CALL two

CONTAINS

  SUBROUTINE one
    i = 99
    ii = 99
    iii = 999
  END SUBROUTINE

  SUBROUTINE two
    i = 0
    ii = 0
    iii = 0
    CALL one
    IF (i .NE. 0) STOP 1
    IF (ii .NE. 99) STOP 2
    IF (iii .NE. 999) STOP 3
  END SUBROUTINE
END

