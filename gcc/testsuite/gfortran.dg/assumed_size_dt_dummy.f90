! { dg-do compile }
! PR20853 - No array size information for initializer.
! PR24440 - patch for PR20853 caused a segfault at line 12.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
MODULE TEST
  TYPE init
    INTEGER :: I=0
  END TYPE init
CONTAINS
  SUBROUTINE try (A, B) ! { dg-error "cannot have a default initializer" }
    TYPE(init), DIMENSION(*), INTENT(OUT) :: A
    TYPE(init)              , INTENT(OUT) :: B ! PR24440 => segfault
  END SUBROUTINE try
END MODULE TEST

end

