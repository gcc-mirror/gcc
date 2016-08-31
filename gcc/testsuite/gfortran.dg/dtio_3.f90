! { dg-do run }
!
! Functional test of User Defined Derived Type IO.
!
! This tests recursive calls where a derived type has a member that is
! itself.
!
MODULE p
  USE ISO_FORTRAN_ENV
  TYPE :: person
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
    type(person), pointer :: next => NULL()
    CONTAINS
      procedure :: pwf
      procedure :: prf
      GENERIC :: WRITE(FORMATTED) => pwf
      GENERIC :: READ(FORMATTED) => prf
  END TYPE person
CONTAINS
  RECURSIVE SUBROUTINE pwf (dtv,unit,iotype,vlist,iostat,iomsg)
    CLASS(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    CHARACTER (LEN=30) :: udfmt
    INTEGER :: myios

    udfmt='(*(g0))'
    iomsg = "SUCCESS"
    iostat=0
    if (iotype.eq."DT") then
      if (size(vlist).ne.0) print *, 36
      if (associated(dtv%next)) then
        WRITE(unit, FMT = '(a20,i2, DT)', IOSTAT=iostat, advance='no') dtv%name, dtv%age, dtv%next
      else
        WRITE(unit, FMT = '(a20,i2)', IOSTAT=iostat, advance='no') dtv%name, dtv%age
      endif
      if (iostat.ne.0) iomsg = "Fail PWF DT"
    endif
    if (iotype.eq."DTzeroth") then
      if (size(vlist).ne.0) print *, 40
      WRITE(unit, FMT = '(g0,g0)', advance='no') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DTzeroth"
    endif
    if (iotype.eq."DTtwo") then
      if (size(vlist).ne.2) call abort
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
      if (associated(dtv%next)) then
        WRITE(unit, FMT = *) dtv%name, dtv%age, dtv%next
      else
        WRITE(unit, FMT = *) dtv%name, dtv%age
      endif
      if (iostat.ne.0) iomsg = "Fail PWF LISTDIRECTED"
    endif
    if (iotype.eq."NAMELIST") then
      if (size(vlist).ne.0) print *, 59
      iostat=6000
    endif
    if (associated (dtv%next) .and. (iotype.eq."LISTDIRECTED")) write(unit, fmt = *) dtv%next
  END SUBROUTINE pwf

  RECURSIVE SUBROUTINE prf (dtv,unit,iotype,vlist,iostat,iomsg)
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
    iomsg = "SUCCESS"
    iostat=0
    if (iotype.eq."DT") then
      if (size(vlist).ne.0) print *, 36
      if (associated(dtv%next)) then
        READ(unit, FMT = '(a20,i2, DT)', IOSTAT=iostat, advance='no') dtv%name, dtv%age, dtv%next
      else
        READ(unit, FMT = '(a20,i2)', IOSTAT=iostat, advance='no') dtv%name, dtv%age
      endif
      if (iostat.ne.0) iomsg = "Fail PWF DT"
    endif
    if (iotype.eq."DTzeroth") then
      if (size(vlist).ne.0) print *, 40
      READ(unit, FMT = '(a,I2)', advance='no') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DTzeroth"
    endif
    if (iotype.eq."DTtwo") then
      if (size(vlist).ne.2) call abort
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
    endif
    !READ (UNIT = UNIT, FMT = *) dtv%name, dtv%age
  END SUBROUTINE prf

END MODULE p

PROGRAM test
  USE p
  TYPE (person) :: chairman
  TYPE (person), target :: member
  character(80) :: astring
  integer :: thelength

  chairman%name="Charlie"
  chairman%age=62
  member%name="George"
  member%age=42
  astring = "FAILURE"
  ! At this point, next is NULL as defined up in the type block.
  open(10, status = "scratch")
  write (10, *, iostat=myiostat, iomsg=astring) member, chairman
  write(10,*)
  rewind(10)
  chairman%name="bogus1"
  chairman%age=99
  member%name="bogus2"
  member%age=66
  read (10, *, iostat=myiostat, iomsg=astring) member, chairman
  if (astring.ne."SUCCESS") print *, astring
  if (member%name.ne."George") call abort
  if (chairman%name.ne."Charlie") call abort
  if (member%age.ne.42) call abort
  if (chairman%age.ne.62) call abort
  close(10, status='delete')
  ! Now we set next to point to member. This changes the code path
  ! in the pwf and prf procedures.
  chairman%next => member
  open(10, status = "scratch")
  write (10,"(DT)") chairman
  rewind(10)
  chairman%name="bogus1"
  chairman%age=99
  member%name="bogus2"
  member%age=66
  read (10,"(DT)", iomsg=astring) chairman
  !print *, trim(astring)
  if (member%name.ne."George") call abort
  if (chairman%name.ne."Charlie") call abort
  if (member%age.ne.42) call abort
  if (chairman%age.ne.62) call abort
  close(10)
END PROGRAM test
