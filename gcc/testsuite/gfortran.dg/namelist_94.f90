! { dg-do compile }
! PR78659 Spurious "requires DTIO" reported against namelist statement
MODULE m
  IMPLICIT NONE
  TYPE :: t
    CHARACTER :: c
  CONTAINS
    PROCEDURE :: write_formatted
    GENERIC :: WRITE(FORMATTED) => write_formatted
  END TYPE
CONTAINS
  SUBROUTINE write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    CLASS(t), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: v_list(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    WRITE (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) dtv%c
    print *, "what"
  END SUBROUTINE
END MODULE

PROGRAM p
  USE m
  IMPLICIT NONE
  class(t), allocatable :: x
  NAMELIST /nml/ x
  x = t('a')
  WRITE (*, nml)
  READ (*, nml) ! { dg-error "is polymorphic and requires a defined input/output procedure" }
END
