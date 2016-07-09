! { dg-do compile }
! PR 71783 - this used to ICE due to a missing charlen for the temporary.
! Test case by Toon Moene.

SUBROUTINE prtdata(ilen)
  INTEGER :: ilen
  character(len=ilen), allocatable :: cline(:)
  allocate(cline(2))
  cline(1) = 'a'
  cline(2) = cline(1)
END SUBROUTINE prtdata
