! { dg-do compile }
! Tests the fix for PR30660 in which gfortran insisted that g_dest
! should have the SAVE attribute because the hidden default
! initializer for the allocatable component was being detected.
!
! Contributed by Toon Moene <toon@moene.indiv.nluug.nl>
!
MODULE types_m
  TYPE coord_t
    INTEGER ncord
    REAL,ALLOCATABLE,DIMENSION(:) :: x, y
  END TYPE

  TYPE grib_t
    REAL,DIMENSION(:),ALLOCATABLE :: vdata
   TYPE(coord_t) coords
  END TYPE
END MODULE

MODULE globals_m
  USE types_m
  TYPE(grib_t) g_dest           ! output field
END MODULE
! { dg-final { cleanup-modules "types_m globals_m" } }

