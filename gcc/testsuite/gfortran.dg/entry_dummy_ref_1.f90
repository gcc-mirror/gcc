! { dg-do compile }
! Tests fix for PR25090 in which references in specification
! expressions to variables that were not entry formal arguments
! would be missed.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
   SUBROUTINE S1(I)
   CHARACTER(LEN=I+J) :: a
   real :: x(i:j), z
   a = ""  ! { dg-error "before the ENTRY statement in which it is a parameter" }
   x = 0.0 ! { dg-error "before the ENTRY statement in which it is a parameter" }
   ENTRY E1(J)
   END SUBROUTINE S1
   END
