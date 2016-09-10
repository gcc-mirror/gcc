! { dg-do compile }
MODULE test_equivalence
  REAL, PRIVATE, DIMENSION(100) :: array1
  REAL, PRIVATE, DIMENSION(100) :: array2
  EQUIVALENCE(array1(1),array2(1))
END MODULE test_equivalence

MODULE mymodule
  USE test_equivalence
  ! declare a local variable with the same name as the (private!)
  ! variable in module test_equivalence:
  REAL, DIMENSION(:), ALLOCATABLE :: array1
END MODULE mymodule

PROGRAM test
  USE mymodule
END PROGRAM test

