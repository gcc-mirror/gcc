! { dg-do run { target fd_truncate } }
!
! Functional test of User Defined Derived Type IO, Formatted WRITE/READ
!
! 1) Tests passing of iostat out of the user procedure.
! 2) Tests parsing of the DT optional string and passing in and using
!    to control execution.
! 3) Tests parsing of the optional vlist, passing in and using it to
!    generate a user defined format string.
! 4) Tests passing an iostat or iomsg out of the libgfortran child
!    procedure back to the parent.
!
MODULE p
  USE ISO_FORTRAN_ENV
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
    CHARACTER (LEN=30) :: udfmt
    INTEGER :: myios

    udfmt='(*(g0))'
    iostat=0
    if (iotype.eq."DT") then
      if (size(vlist).ne.0) print *, 36
      WRITE(unit, FMT = '(a,5x,i2)', IOSTAT=iostat, advance='no') trim(dtv%name), dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DT"
    endif
    if (iotype.eq."DTzeroth") then
      if (size(vlist).ne.0) print *, 40
      WRITE(unit, FMT = '(g0,g0)', advance='no') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DTzeroth"
    endif
    if (iotype.eq."DTtwo") then
      if (size(vlist).ne.2) STOP 1
      WRITE(udfmt,'(A,A,I1,A,I1,A)') '(', 'A', vlist(1),',I', vlist(2), ')'
      WRITE(unit, FMT='(A8,I2)') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DTtwo"
    endif
    if (iotype.eq."DTthree") then
      WRITE(udfmt,'(2A,I2,A,I1,A,I2,A)',iostat=myios) '(', 'A', vlist(1),',I', vlist(2), ',F', vlist(3), '.2)'
      WRITE(unit, FMT=udfmt, IOSTAT=iostat, advance='no') trim(dtv%name), dtv%age, 3.14
      if (iostat.ne.0) iomsg = "Fail PWF DTthree"
    endif
    if (iotype.eq."LISTDIRECTED") then
      if (size(vlist).ne.0) print *, 55
      WRITE(unit, FMT = *) dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF LISTDIRECTED"
    endif
    if (iotype.eq."NAMELIST") then
      if (size(vlist).ne.0) print *, 59
      iostat=6000
      iomsg = "NAMELIST not implemented in pwf"
    endif
  END SUBROUTINE pwf

  SUBROUTINE prf (dtv,unit,iotype,vlist,iostat,iomsg)
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    CHARACTER (LEN=30) :: udfmt
    INTEGER :: myios
    real :: areal
    udfmt='(*(g0))'
    iostat=0
    if (iotype.eq."DT") then
      if (size(vlist).ne.0) print *, 36
      READ(unit, FMT = '(a,5x,i2)', IOSTAT=iostat, advance='no') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DT"
    endif
    if (iotype.eq."DTzeroth") then
      if (size(vlist).ne.0) print *, 40
      READ(unit, FMT = '(a,I2)', advance='no') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DTzeroth"
    endif
    if (iotype.eq."DTtwo") then
      if (size(vlist).ne.2) STOP 2
      WRITE(udfmt,'(A,A,I1,A,I1,A)') '(', 'A', vlist(1),',I', vlist(2), ')'
      READ(unit, FMT='(A8,I2)') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DTtwo"
    endif
    if (iotype.eq."DTthree") then
      WRITE(udfmt,'(2A,I2,A,I1,A,I2,A)',iostat=myios) '(', 'A', vlist(1),',I', vlist(2), ',F', vlist(3), '.2)'
      READ(unit, FMT=udfmt, IOSTAT=iostat, advance='no') dtv%name, dtv%age, areal
      if (iostat.ne.0) iomsg = "Fail PWF DTthree"
    endif
    if (iotype.eq."LISTDIRECTED") then
      if (size(vlist).ne.0) print *, 55
      READ(unit, FMT = *) dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF LISTDIRECTED"
    endif
    if (iotype.eq."NAMELIST") then
      if (size(vlist).ne.0) print *, 59
      iostat=6000
      iomsg = "NAMELIST not implemented in prf"
    endif
  END SUBROUTINE prf

END MODULE p

PROGRAM test
  USE p
  TYPE (person), SAVE :: chairman
  TYPE (person), SAVE :: member
  character(80) :: astring
  integer :: thelength

  chairman%name="Charlie"
  chairman%age=62
  member%name="George"
  member%age=42
  astring = "SUCCESS"
  write (10, "(DT'zeroth',3x, DT'three'(11,4,10),11x,DT'two'(8,2))", &
         & iostat=myiostat, iomsg=astring) member, chairman, member
  if (myiostat.ne.0) STOP 3
  if (astring.ne."SUCCESS") STOP 4
  astring = "SUCCESS"
  write (10, *, iostat=myiostat, iomsg=astring) member, chairman, member
  if (myiostat.ne.0) STOP 5
  if (astring.ne."SUCCESS") STOP 6
  write(10,*) ! See note below
  rewind(10)
  chairman%name="bogus1"
  chairman%age=99
  member%name="bogus2"
  member%age=66
  astring = "SUCCESS"
  read(10,"(DT'zeroth',3x, DT'three'(11,4,10),11x,DT'two'(8,2))") member, chairman, member
  if (member%name.ne."George") STOP 7
  if (chairman%name.ne."    Charlie") STOP 8
  if (member%age.ne.42) STOP 9
  if (chairman%age.ne.62) STOP 10
  chairman%name="bogus1"
  chairman%age=99
  member%name="bogus2"
  member%age=66
  astring = "SAME"
  read (10, *, iostat=myiostat, iomsg=astring) member, chairman, member
  ! The user defined procedure reads to the end of the line/file, then finalizing the parent
  ! reads past, so we wrote a blank line above. User needs to address these nuances in their
  ! procedures. (subject to interpretation)
  if (astring.ne."SAME" .or. myiostat.ne.0) STOP 11
  if (member%name.ne."George") STOP 12
  if (chairman%name.ne."Charlie") STOP 13
  if (member%age.ne.42) STOP 14
  if (chairman%age.ne.62) STOP 15
END PROGRAM test
