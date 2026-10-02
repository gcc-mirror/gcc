! { dg-do compile }
!
! PR fortran/127683 - we used to ICE on invalid inquiry refs

program test
  implicit none
  print *, z%re   ! { dg-error "must be applied to a COMPLEX expression" }
  print *, c%len  ! { dg-error "must be applied to a CHARACTER expression" }
! print *, x%kind ! this also used to ICE, now gives a fatal error
end program
