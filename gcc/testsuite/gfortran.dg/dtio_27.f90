! { dg-do run }
!
! PR 78661: [OOP] Namelist output missing object designator under DTIO
!
! Contributed by Ian Harvey <ian_harvey@bigpond.com>

MODULE m
  IMPLICIT NONE
  TYPE :: t
    CHARACTER :: c
  CONTAINS
    PROCEDURE :: write_formatted
    GENERIC :: WRITE(FORMATTED) => write_formatted
    PROCEDURE :: read_formatted
    GENERIC :: READ(FORMATTED) => read_formatted
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
  END SUBROUTINE
  SUBROUTINE read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    CLASS(t), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: v_list(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) dtv%c
  END SUBROUTINE
END MODULE


PROGRAM p

  USE m
  IMPLICIT NONE
  character(len=4), dimension(3) :: buffer
  call test_type
  call test_class

contains

  subroutine test_type
    type(t) :: x
    namelist /n1/ x
    x = t('a')
    write (buffer, n1)
    if (buffer(2) /= " X=a") STOP 1
  end subroutine

  subroutine test_class
    class(t), allocatable :: y
    namelist /n2/ y
    y = t('b')
    write (buffer, n2)
    if (buffer(2) /= " Y=b") STOP 2
  end subroutine

END
