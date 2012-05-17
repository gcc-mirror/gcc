! { dg-do run }
!
! PR fortran/38657, in which the mixture of PRIVATE and
! COMMON in TEST4, would mess up the association with
! TESTCHAR in TEST2.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
! From a report in clf by Chris Bradley.
!
MODULE TEST4
  PRIVATE
  CHARACTER(LEN=80) :: T1 = &
    "Mary had a little lamb, Its fleece was white as snow;"
  CHARACTER(LEN=80) :: T2 = &
    "And everywhere that Mary went, The lamb was sure to go."
  CHARACTER(LEN=80) :: TESTCHAR
  COMMON /TESTCOMMON1/ TESTCHAR
  PUBLIC T1, T2, FOOBAR
CONTAINS
  subroutine FOOBAR (CHECK)
    CHARACTER(LEN=80) :: CHECK
    IF (TESTCHAR .NE. CHECK) CALL ABORT
  end subroutine
END MODULE TEST4

MODULE TEST3
  CHARACTER(LEN=80) :: TESTCHAR
  COMMON /TESTCOMMON1/ TESTCHAR
END MODULE TEST3

MODULE TEST2
  use TEST4
  USE TEST3, chr => testchar
  PRIVATE
  CHARACTER(LEN=80) :: TESTCHAR
  COMMON /TESTCOMMON1/ TESTCHAR
  PUBLIC TESTCHAR, FOO, BAR, CHR, T1, T2, FOOBAR
contains
  subroutine FOO
    TESTCHAR = T1
  end subroutine
  subroutine BAR (CHECK)
    CHARACTER(LEN=80) :: CHECK
    IF (TESTCHAR .NE. CHECK) CALL ABORT
    IF (CHR .NE. CHECK) CALL ABORT
  end subroutine
END MODULE TEST2

PROGRAM TEST1
  USE TEST2
  call FOO
  call BAR (T1)
  TESTCHAR = T2
  call BAR (T2)
  CALL FOOBAR (T2)
END PROGRAM TEST1
