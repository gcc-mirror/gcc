! { dg-do compile }
!
! PR fortran/99146
!
      MODULE p
      TYPE :: person
      sequence
      END TYPE person
      INTERFACE READ(UNFORMATTED)
       MODULE PROCEDURE pruf
      END INTERFACE

      CONTAINS

      SUBROUTINE pruf (dtv,unit,iostat,iomsg)
       type(person), INTENT(INOUT) :: dtv
       INTEGER, INTENT(IN) :: unit
       INTEGER, INTENT(OUT) :: iostat
       CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
       iostat = 1
      END SUBROUTINE pruf

      END MODULE p

      PROGRAM test
      USE p
      TYPE (person) :: chairman

      OPEN (UNIT=71, status = 'scratch', FORM='UNFORMATTED')

      read(71) chairman

      END PROGRAM test
