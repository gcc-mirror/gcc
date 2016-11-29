! { dg-do run }
! Tests that inquire(iolength=) treats derived types as if they do not
! have User Defined procedures. Fortran Draft F2016 Standard, 9.10.3
MODULE p
  TYPE :: person
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
  END TYPE person
  INTERFACE WRITE(FORMATTED)
     MODULE procedure pwf
  END INTERFACE
  INTERFACE WRITE(UNFORMATTED)
     MODULE procedure pwuf
  END INTERFACE
  INTERFACE read(FORMATTED)
     MODULE procedure prf
  END INTERFACE
  INTERFACE read(UNFORMATTED)
     MODULE procedure pruf
  END INTERFACE
CONTAINS
  SUBROUTINE pwf (dtv,unit,iotype,vlist,iostat,iomsg)
    CLASS(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE(unit, FMT = *, IOSTAT=iostat) dtv%name, dtv%age
  END SUBROUTINE pwf

  SUBROUTINE prf (dtv,unit,iotype,vlist,iostat,iomsg)
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    READ (UNIT = UNIT, FMT = *) dtv%name, dtv%age
  END SUBROUTINE prf

  SUBROUTINE pwuf (dtv,unit,iostat,iomsg)
    CLASS(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    print *, "in pwuf"
    WRITE (UNIT=UNIT, FMT = *) DTV%name, DTV%age
  END SUBROUTINE pwuf

  SUBROUTINE pruf (dtv,unit,iostat,iomsg)
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    print *, "in pruf"
    READ (UNIT = UNIT, FMT = *) dtv%name, dtv%age
  END SUBROUTINE pruf

END MODULE p

PROGRAM test
  USE p
  IMPLICIT NONE
  TYPE (person) :: chairman
  integer(4) :: rl, tl, kl

  chairman%name="Charlie"
  chairman%age=62

  inquire(iolength=rl) rl, kl, chairman, rl, chairman, tl
  if (rl.ne.64) call abort
END PROGRAM test
