! { dg-do compile }
! PR20853 - No array size information for initializer.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
MODULE TEST
TYPE init
INTEGER :: I=0
END TYPE init
CONTAINS
SUBROUTINE try(A) ! { dg-error "cannot have a default initializer" }
  TYPE(init), DIMENSION(*), INTENT(OUT) :: A
END SUBROUTINE try
END MODULE TEST
END
