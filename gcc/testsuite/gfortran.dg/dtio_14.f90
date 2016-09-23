! { dg-do run }
!
! Functional test of User Defined Derived Type IO with typebound bindings
! This version tests IO to internal character units.
!
MODULE p
  TYPE :: person
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
    CONTAINS
      procedure :: pwf
      procedure :: prf
      GENERIC :: WRITE(FORMATTED) => pwf
      GENERIC :: READ(FORMATTED) => prf
  END TYPE person
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
END MODULE p

PROGRAM test
  USE p
  TYPE (person) :: chairman, answer
  character(kind=1,len=80) :: str1
  character(kind=4,len=80) :: str4
  str1 = ""
  str4 = 4_""
  chairman%name="Charlie"
  chairman%age=62
  answer = chairman
! KIND=1 test
  write (str1, *) chairman
  if (trim(str1).ne."  Charlie                       62") call abort
  chairman%name="Bogus"
  chairman%age=99
  read (str1, *) chairman
  if (chairman%name.ne.answer%name) call abort
  if (chairman%age.ne.answer%age) call abort
! KIND=4 test
  write (str4, *) chairman
  if (trim(str4).ne.4_"  Charlie                       62") call abort
  chairman%name="Bogus"
  chairman%age=99
  read (str4, *) chairman
  if (chairman%name.ne.answer%name) call abort
  if (chairman%age.ne.answer%age) call abort
END PROGRAM test
