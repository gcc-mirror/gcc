! PR 25048
! { dg-do compile }
! Originally contributed by Joost VandeVondele
INTEGER, POINTER :: I
CALL S1((I)) ! { dg-error "Actual argument for .i. must be a pointer" }
CONTAINS
 SUBROUTINE S1(I)
  INTEGER, POINTER ::I
 END SUBROUTINE S1
END

