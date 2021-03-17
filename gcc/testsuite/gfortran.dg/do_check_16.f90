! { dg-do compile }
program main
  implicit none
  integer :: iq,nq,recl
  DO iq = 1, nq
     call foobar  ! { dg-error "redefined" }     
  ENDDO  
CONTAINS

  subroutine foobar
    inquire (iolength=nq) iq ! { dg-error "redefined" }
  end subroutine foobar
END program main
