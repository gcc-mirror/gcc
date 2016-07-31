! { dg-do compile }
! { dg-options "-O0" }
! PR20890 - Equivalence cannot contain overlapping unequal initializers.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
! Started out being in BLOCK DATA; however, blockdata variables must be in
! COMMON and therefore cannot have F95 style initializers....
 MODULE DATA
  INTEGER :: I=1,J=2
  EQUIVALENCE(I,J)  ! { dg-error "Overlapping unequal initializers" }
 END MODULE DATA
 END
