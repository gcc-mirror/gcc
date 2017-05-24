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
  type(ta):: x
  NAMELIST /nml/ x
  WRITE (*, nml) ! { dg-error "has ALLOCATABLE or POINTER components and thus requires a defined input/output" }
  READ (*, nml) ! { dg-error "has ALLOCATABLE or POINTER components and thus requires a defined input/output" }
END PROGRAM p
