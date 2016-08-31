! { dg-do run }
!
! Tests dtio of transfer bind-C types.
!
! Note difficulties with c_char at -O1. This is why no character field is used.
!
MODULE p
  USE ISO_C_BINDING
  TYPE, BIND(C) :: person
    integer(c_int) :: id_no
    INTEGER(c_int) :: age
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
    WRITE (UNIT=UNIT) DTV%id_no, DTV%age
  END SUBROUTINE pwuf

  SUBROUTINE pruf (dtv,unit,iostat,iomsg)
    type(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    READ (UNIT = UNIT) dtv%id_no, dtv%age
  END SUBROUTINE pruf

END MODULE p

PROGRAM test
  USE p
  TYPE (person) :: chairman
  CHARACTER (kind=c_char) :: cname(20)
  integer (c_int) :: cage, cid_no
  character(10) :: line

  cid_no = 1
  cage = 62
  chairman%id_no = cid_no
  chairman%age = cage

  OPEN (UNIT=71, status = 'scratch', FORM='UNFORMATTED')
  write (71) chairman
  rewind (71)

  chairman%id_no = 0
  chairman%age = 0

  read (71) chairman
  close (unit = 71)

  write(line, "(I4)") chairman%id_no
  if (trim (line) .ne. "   1") call abort
  write(line, "(I4)") chairman%age
  if (trim (line) .ne. "  62") call abort
end program
