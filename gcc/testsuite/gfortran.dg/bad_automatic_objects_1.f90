! { dg-do compile }
! Tests the fix for 25103, in which the presence of automatic objects
! in the main program and the specification part of a module was not
! detected.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
! Error message update with patch for PR fortran/83633
!
module foo
  integer    ::  i
end module foo
module bar
  use foo
  integer, dimension (i) :: j ! { dg-error "array with nonconstant bounds" }
  character (len = i) :: c1   ! { dg-error "must have constant character length" }
end module bar
program foobar
  use foo
  integer, dimension (i) :: k ! { dg-error "array with nonconstant bounds" }
  character (len = i) :: c2   ! { dg-error "must have constant character length" }
end program foobar
