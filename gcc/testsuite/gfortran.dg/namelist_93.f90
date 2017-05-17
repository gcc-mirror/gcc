! { dg-do compile }
! PR78659 Spurious "requires DTIO" reported against namelist statement
MODULE ma
  IMPLICIT NONE
  TYPE :: ta
    INTEGER, allocatable :: array(:)
  END TYPE ta
END MODULE ma

PROGRAM p
  USE ma
  class(ta), allocatable :: x
  NAMELIST /nml/ x
  WRITE (*, nml)! { dg-error "is polymorphic and requires a defined input/output procedure" }
  READ (*, nml) ! { dg-error "is polymorphic and requires a defined input/output procedure" }
END PROGRAM p
