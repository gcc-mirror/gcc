! { dg-do run }
! Tests the fix for PR42309, in which the indexing of 'Q'
! was off by one.
!
! Contributed by Gilbert Scott <gilbert.scott@easynet.co.uk>
!
PROGRAM X
  TYPE T
    INTEGER :: I
    REAL :: X
  END TYPE T
  TYPE(T), TARGET :: T1(0:3)
  INTEGER, POINTER :: P(:)
  REAL :: SOURCE(4) = [10., 20., 30., 40.]

  T1%I = [1, 2, 3, 4]
  T1%X = SOURCE
  P => T1%I
  CALL Z(P)
  IF (ANY (T1%I .NE. [999, 2, 999, 4])) CALL ABORT
  IF (ANY (T1%X .NE. SOURCE)) CALL ABORT
CONTAINS
  SUBROUTINE Z(Q)
    INTEGER, POINTER :: Q(:)
    Q(1:3:2) = 999
  END SUBROUTINE Z
END PROGRAM X

