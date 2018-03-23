! { dg-do run }
! PR48298, this tests function of size= specifier with DTIO.
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

    iomsg = "SUCCESS"
    iostat=0
    if (iotype.eq."DT") then
      WRITE(unit, FMT = '(a20,i2)', IOSTAT=iostat, advance='no') dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DT"
    endif
    if (iotype.eq."LISTDIRECTED") then
      WRITE(unit, '(*(g0))', IOSTAT=iostat) dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DT"
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
    iomsg = "SUCCESS"
    iostat=0
    if (iotype.eq."DT") then
      READ(unit, FMT = '(a20,i2)', IOSTAT=iostat) dtv%name, dtv%age
      if (iostat.ne.0) iomsg = "Fail PWF DT"
    endif
  END SUBROUTINE prf

END MODULE p

PROGRAM test
  USE p
  implicit none
  TYPE (person) :: chairman
  integer(4) :: rl, tl, kl, thesize

  rl = 1
  tl = 22
  kl = 333
  thesize = 9999
  chairman%name="Charlie"
  chairman%age=62

  open(28, status='scratch')
  write(28, '(i10,i10,DT,i15,DT,i12)') rl, kl, chairman, rl, chairman, tl
  rewind(28)
  chairman%name="bogus"
  chairman%age=99
  !print *, chairman
  read(28, '(i10,i10,DT,i15,DT,i12)', advance='no', size=thesize) rl, &
                          & kl, chairman, rl, chairman, tl
  if (thesize.ne.91) STOP 1
  close(28)
END PROGRAM test
