! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! PR fortran/44602
! Check for correct behaviour of EXIT / CYCLE combined with non-loop
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
    CALL abort ()
    EXIT myif
  ELSE IF (i == 2) THEN
    EXIT myif
    CALL abort ()
  ELSE
    CALL abort ()
    EXIT myif
  END IF myif

  mysel: SELECT CASE (i)
    CASE (1)
      CALL abort ()
      EXIT mysel
    CASE (2)
      EXIT mysel
      CALL abort ()
    CASE DEFAULT
      CALL abort ()
      EXIT mysel
  END SELECT mysel

  mycharsel: SELECT CASE ("foobar")
    CASE ("abc")
      CALL abort ()
      EXIT mycharsel
    CASE ("xyz")
      CALL abort ()
      EXIT mycharsel
    CASE DEFAULT
      EXIT mycharsel
      CALL abort ()
  END SELECT mycharsel

  myblock: BLOCK
    EXIT myblock
    CALL abort ()
  END BLOCK myblock

  myassoc: ASSOCIATE (x => 5 + 2)
    EXIT myassoc
    CALL abort ()
  END ASSOCIATE myassoc

  ALLOCATE (t :: var)
  mytypesel: SELECT TYPE (var)
    TYPE IS (t)
      EXIT mytypesel
      CALL abort ()
    CLASS DEFAULT
      CALL abort ()
      EXIT mytypesel
  END SELECT mytypesel

  ! Check EXIT with nested constructs.
  outer: BLOCK
    inner: IF (.TRUE.) THEN
      EXIT outer
      CALL abort ()
    END IF inner
    CALL abort ()
  END BLOCK outer
END PROGRAM main
