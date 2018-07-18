! { dg-do compile }
!
! Tests the checks for interface compliance.
!
!
MODULE p
  USE ISO_C_BINDING

  TYPE :: person
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
    CONTAINS
      procedure :: pwf ! { dg-error "Non-polymorphic passed-object" }
      procedure :: pwuf
      GENERIC :: WRITE(FORMATTED) => pwf
      GENERIC :: WRITE(UNFORMATTED) => pwuf
  END TYPE person
  INTERFACE READ(FORMATTED)
    MODULE PROCEDURE prf
  END INTERFACE
  INTERFACE READ(UNFORMATTED)
    MODULE PROCEDURE pruf
  END INTERFACE

  TYPE :: seq_type
    sequence
    INTEGER(4) :: i
  END TYPE seq_type
  INTERFACE WRITE(FORMATTED)
    MODULE PROCEDURE pwf_seq
  END INTERFACE

  TYPE, BIND(C) :: bindc_type
    INTEGER(C_INT) :: i
  END TYPE bindc_type

  INTERFACE WRITE(FORMATTED)
    MODULE PROCEDURE pwf_bindc
  END INTERFACE

CONTAINS
  SUBROUTINE pwf (dtv,unit,iotype,vlist,iostat,iomsg) ! { dg-error "must be of type CLASS" }
    type(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE(unit, FMT = *, IOSTAT=iostat) dtv%name, dtv%age
  END SUBROUTINE pwf

  SUBROUTINE prf (dtv,unit,iotype,vlist,iostat,iomsg) ! { dg-error "must be an ASSUMED SHAPE ARRAY" }
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    READ (UNIT = UNIT, FMT = *) dtv%name, dtv%age
  END SUBROUTINE prf

  SUBROUTINE pwuf (dtv,unit,iostat,iomsg)  ! { dg-error "must have INTENT IN" }
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE (UNIT=UNIT, FMT = *) DTV%name, DTV%age
  END SUBROUTINE pwuf

  SUBROUTINE pruf (dtv,unit,iostat,iomsg)  ! { dg-error "must be of KIND = 4" }
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER(8), INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    READ (UNIT = UNIT, FMT = *) dtv%name, dtv%age
  END SUBROUTINE pruf

  SUBROUTINE pwf_seq (dtv,unit,iotype,vlist,iostat,iomsg) ! { dg-error "not extensible|DERIVED" }
    class(seq_type), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE(unit, FMT = *, IOSTAT=iostat) dtv%i
  END SUBROUTINE pwf_seq

  SUBROUTINE pwf_bindc (dtv,unit,iotype,vlist,iostat,iomsg) ! { dg-error "not extensible|DERIVED" }
    class(bindc_type), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE(unit, FMT = *, IOSTAT=iostat) dtv%i
  END SUBROUTINE pwf_bindc

END MODULE p
