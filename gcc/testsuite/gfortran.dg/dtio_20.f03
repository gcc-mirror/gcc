MODULE m
  IMPLICIT NONE
  
  TYPE :: t
    CHARACTER :: c
  CONTAINS
    PROCEDURE :: write_formatted
    GENERIC :: WRITE(FORMATTED) => write_formatted
  END TYPE t
CONTAINS
  SUBROUTINE write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    CLASS(t), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: v_list(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    
    WRITE (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) iotype
  END SUBROUTINE write_formatted
END MODULE m

PROGRAM p
  USE m
  IMPLICIT NONE
  CHARACTER(25) :: str
  
  TYPE(t) :: x
  WRITE (str, "(DT'a''b')") x
  if (str.ne."DTa'b") STOP 1
END PROGRAM p
