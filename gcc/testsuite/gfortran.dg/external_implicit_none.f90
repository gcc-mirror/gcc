! { dg-do compile }
! Tests fix for PR18737 - ICE on external symbol of unknown type.
program test
  implicit none
  real(8) :: x
  external bug  ! { dg-error "has no IMPLICIT type" }

  x = 2
  print *, bug(x)
  
end program test