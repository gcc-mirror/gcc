! { dg-do compile }
! PR16404 test 3 and PR20835 - Target cannot be equivalence object.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
  REAL :: A
  REAL, TARGET :: B
  EQUIVALENCE(A,B) ! { dg-error "conflicts with TARGET attribute" }
END

