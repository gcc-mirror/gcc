! { dg-do compile }
! Tests the fix for PR28771 in which an assumed character length variable with an initializer could
! survive in the main program without causing an error.
!
! Contributed by Martin Reinecke  <martin@mpa-garching.mpg.de>
! Modified to test fix of regression reported by P.Schaffnit@access.rwth-aachen.de

subroutine poobar ()
  ! The regression caused an ICE here
  CHARACTER ( LEN = * ), PARAMETER ::   Markers(5) = (/ "Error ", &
      &                                                 "Fehler", &
      &                                                 "Erreur", &
      &                                                 "Stop  ", &
      &                                                 "Arret "  /)
  character(6) :: recepteur (5)
  recepteur = Markers
end subroutine poobar

! If the regression persisted, the compilation would stop before getting here
program test
  character(len=*), parameter :: foo = 'test'     ! Parameters must work.
  character(len=4) :: bar = foo
  character(len=*) :: foobar = 'This should fail' ! { dg-error "must be a dummy" }
  print *, bar
  call poobar ()
end

