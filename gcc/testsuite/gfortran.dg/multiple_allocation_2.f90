! { dg-do run }
! PR 27470: This used fail because of confusion between
!           mol (allocatable) and mol(1)%array(:) (pointer).
!           Derived from a test case by FX Coudert.
PROGRAM MAIN
  TYPE foo
    INTEGER, DIMENSION(:), POINTER :: array
  END TYPE foo

  type(foo),allocatable,dimension(:) :: mol

  ALLOCATE (mol(1))
  ALLOCATE (mol(1)%array(5))
  ALLOCATE (mol(1)%array(5))

  END
