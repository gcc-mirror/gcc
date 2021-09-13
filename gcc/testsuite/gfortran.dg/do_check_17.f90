! { dg-do compile }
! PR 99345 - this used to cause an ICE.
! Original test case by Matthias Klose
program main
  implicit none
  integer :: iq,nq,recl
  DO iq = 1, nq
     CALL calc_upper_fan (iq)
  ENDDO  
CONTAINS
  SUBROUTINE calc_upper_fan (iq)
    INTEGER :: iq
    INTEGER :: recl
    INQUIRE(IOLENGTH=recl) iq
  END SUBROUTINE calc_upper_fan
END
