! { dg-do compile }
!
! PR fortran/51448
!
! Contribued by François Willot
!
  PROGRAM MAIN
  IMPLICIT NONE
  TYPE mytype
    REAL b(2)
  END TYPE mytype
  TYPE(mytype) a
  DOUBLE PRECISION, ALLOCATABLE :: x(:)
  ALLOCATE(x(2))
  a%b=0.0E0
  x=a%b
  END
