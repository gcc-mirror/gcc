! { dg-do run }
! PR78670 Incorrect file position with namelist read under DTIO
MODULE m
  IMPLICIT NONE
  TYPE :: t
    CHARACTER :: c
  CONTAINS
    PROCEDURE :: read_formatted
    GENERIC :: READ(FORMATTED) => read_formatted
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
    write(unit,'(a)', iostat=iostat, iomsg=iomsg) dtv%c
  END SUBROUTINE write_formatted
  
  SUBROUTINE read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    CLASS(t), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: v_list(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    
    CHARACTER :: ch
    dtv%c = ''
    DO
      READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) ch
      IF (iostat /= 0) RETURN
      ! Store first non-blank
      IF (ch /= ' ') THEN
        dtv%c = ch
        RETURN
      END IF
    END DO
  END SUBROUTINE read_formatted
END MODULE m

PROGRAM p
  USE m
  IMPLICIT NONE
  TYPE(t) :: x
  TYPE(t) :: y
  TYPE(t) :: z
  integer :: j, k
  NAMELIST /nml/ j, x, y, z, k
  INTEGER :: unit, iostatus
  
  OPEN(NEWUNIT=unit, STATUS='SCRATCH', ACTION='READWRITE')
  
  x%c = 'a'
  y%c = 'b'
  z%c = 'c'
  j=1
  k=2
  WRITE(unit, nml)
  REWIND (unit)
  x%c = 'x'
  y%c = 'y'
  z%c = 'x'
  j=99
  k=99
  READ (unit, nml, iostat=iostatus)
  if (iostatus.ne.0) call abort
  if (j.ne.1 .or. k.ne.2 .or. x%c.ne.'a' .or. y%c.ne.'b' .or. z%c.ne.'c') call abort
  !WRITE(*, nml)
END PROGRAM p
