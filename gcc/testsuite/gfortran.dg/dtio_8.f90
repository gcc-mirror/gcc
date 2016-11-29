! { dg-do run }
!
! Tests dtio transfer sequence types.
!
! Note difficulty at end with comparisons at any level of optimization.
!
MODULE p
  TYPE :: person
    sequence
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
  END TYPE person
  INTERFACE WRITE(UNFORMATTED)
    MODULE PROCEDURE pwuf
  END INTERFACE
  INTERFACE READ(UNFORMATTED)
    MODULE PROCEDURE pruf
  END INTERFACE

CONTAINS

  SUBROUTINE pwuf (dtv,unit,iostat,iomsg)
    type(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE (UNIT=UNIT) DTV%name, DTV%age
  END SUBROUTINE pwuf

  SUBROUTINE pruf (dtv,unit,iostat,iomsg)
    type(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    READ (UNIT = UNIT) dtv%name, dtv%age
  END SUBROUTINE pruf

END MODULE p

PROGRAM test
  USE p
  TYPE (person) :: chairman
  character(10) :: line

  chairman%name="Charlie"
  chairman%age=62

  OPEN (UNIT=71, status = 'scratch', FORM='UNFORMATTED')
  write (71) chairman
  rewind (71)

  chairman%name = "Charles"
  chairman%age = 0

  read (71) chairman
  close (unit = 71)

! Straight comparisons fail at any level of optimization.

  write(line, "(A7)") chairman%name
  if (trim (line) .ne. "Charlie") call abort
  line = "          "
  write(line, "(I4)") chairman%age
  if (trim (line) .eq. "   62") print *, trim(line)
END PROGRAM test
