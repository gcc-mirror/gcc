! { dg-lto-do link }
!
! PR fortran/45586 (comment 53)
!

MODULE M1
  INTEGER, PARAMETER :: dp=8
  TYPE realspace_grid_type
     REAL(KIND=dp), DIMENSION ( :, :, : ), ALLOCATABLE :: r
  END TYPE realspace_grid_type
  TYPE realspace_grid_p_type
     TYPE(realspace_grid_type), POINTER :: rs_grid
  END TYPE realspace_grid_p_type
  TYPE realspaces_grid_p_type
     TYPE(realspace_grid_p_type), DIMENSION(:), POINTER :: rs
  END TYPE realspaces_grid_p_type
END MODULE

MODULE M2
 USE M1
CONTAINS
 SUBROUTINE S1()
  INTEGER :: i,j
  TYPE(realspaces_grid_p_type), DIMENSION(:), POINTER :: rs_gauge
  REAL(dp), DIMENSION(:, :, :), POINTER    :: y
  y=>rs_gauge(i)%rs(j)%rs_grid%r
 END SUBROUTINE
END MODULE

USE M2
  CALL S1()
END

! { dg-final { cleanup-modules "m1 m2" } }
