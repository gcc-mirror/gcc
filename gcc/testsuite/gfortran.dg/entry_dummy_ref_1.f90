! { dg-do compile }
! Tests fix for PR25090 in which references in specification
! expressions to variables that were not entry formal arguments
! would be missed.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
   SUBROUTINE S1(I) ! { dg-error "must be a parameter of the entry" }
   CHARACTER(LEN=I+J) :: a ! { dg-error "must be a parameter of the entry" }
   real :: x(i:j) ! { dg-error "must be a parameter of the entry" }
   ENTRY E1(J) ! { dg-error "must be a parameter of the entry" }
   END SUBROUTINE S1
   END
