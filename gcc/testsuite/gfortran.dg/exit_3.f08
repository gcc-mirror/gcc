! { dg-do run }
! { dg-options "-std=f2008 " }

! PR fortran/44602
! Check for correct behavior of EXIT / CYCLE combined with non-loop
! constructs at run-time.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  TYPE :: t
  END TYPE t

  INTEGER :: i
  CLASS(t), ALLOCATABLE :: var

  ! EXIT and CYCLE without names always refer to innermost *loop*.  This
  ! however is checked at run-time already in exit_1.f08.

  ! Basic EXITs from different non-loop constructs.

  i = 2
  myif: IF (i == 1) THEN
    STOP 1
    EXIT myif
  ELSE IF (i == 2) THEN
    EXIT myif
    STOP 2
  ELSE
    STOP 3
    EXIT myif
  END IF myif

  mysel: SELECT CASE (i)
    CASE (1)
      STOP 4
      EXIT mysel
    CASE (2)
      EXIT mysel
      STOP 5
    CASE DEFAULT
      STOP 6
      EXIT mysel
  END SELECT mysel

  mycharsel: SELECT CASE ("foobar")
    CASE ("abc")
      STOP 7
      EXIT mycharsel
    CASE ("xyz")
      STOP 8
      EXIT mycharsel
    CASE DEFAULT
      EXIT mycharsel
      STOP 9
  END SELECT mycharsel

  myblock: BLOCK
    EXIT myblock
    STOP 10
  END BLOCK myblock

  myassoc: ASSOCIATE (x => 5 + 2)
    EXIT myassoc
    STOP 11
  END ASSOCIATE myassoc

  ALLOCATE (t :: var)
  mytypesel: SELECT TYPE (var)
    TYPE IS (t)
      EXIT mytypesel
      STOP 12
    CLASS DEFAULT
      STOP 13
      EXIT mytypesel
  END SELECT mytypesel

  ! Check EXIT with nested constructs.
  outer: BLOCK
    inner: IF (.TRUE.) THEN
      EXIT outer
      STOP 14
    END IF inner
    STOP 15
  END BLOCK outer
END PROGRAM main
