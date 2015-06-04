! { dg-do  run }
! PR 52749 - this used to ICE.
! Original test case by Stefan Mauerberger.
PROGRAM test
  character(len=10) :: u
  WRITE(unit=u,fmt='(3A)') PACK(ADJUSTL([" a", " b"]), [.TRUE., .FALSE.])
  if (u .ne. 'a    ') call abort
END PROGRAM test
