! { dg-do run { target fd_truncate } }
! { dg-options "-O0" }
! Test patch for PR24416.f90 - a used to come back from the read with var
! prepended.
!
  IMPLICIT NONE
  CHARACTER(len=10)  :: var = "hello"
  character(len=10)  :: a = ""
  NAMELIST /inx/ var

  OPEN(unit=11, status='scratch')
  write (11, *) "&INX"
  write (11, *) "  var = 'goodbye'"
  write (11, *) "&END"
  rewind (11)

  READ(11,NML=inx)
  CLOSE(11)

  OPEN(unit=11, status='scratch')
  write (11, *) "alls_well"
  rewind (11)

  READ(11,*) a
  CLOSE(11)

  if (a /= "alls_well") call abort ()

END
