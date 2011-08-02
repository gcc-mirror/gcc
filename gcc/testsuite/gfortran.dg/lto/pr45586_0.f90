! { dg-lto-do link }
      MODULE M1
      INTEGER, PARAMETER :: dp=8
      TYPE realspace_grid_type

          REAL(KIND=dp), DIMENSION ( :, :, : ), ALLOCATABLE :: r

      END TYPE realspace_grid_type
      END MODULE

      MODULE M2
      USE m1
      CONTAINS
      SUBROUTINE S1(x)
      TYPE(realspace_grid_type), POINTER :: x
      REAL(dp), DIMENSION(:, :, :), POINTER    :: y
      y=>x%r
      y=0

      END SUBROUTINE
      END MODULE

      USE M2
      TYPE(realspace_grid_type), POINTER :: x
      ALLOCATE(x)
      ALLOCATE(x%r(10,10,10))
      CALL S1(x)
      write(6,*) x%r
      END

! { dg-final { cleanup-modules "m1 m2" } }
