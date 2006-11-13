! PR fortran/29759
! { dg-do compile }

PROGRAM test_omp
!$OMP PARALLEL &
!$OMP NUM_THREADS(2)
!$OMP END PARALLEL

!$OMP PARALLEL &
!$OMP & NUM_THREADS(2)
!$OMP END PARALLEL

!$OMP PARALLEL &
!
!$OMP NUM_THREADS(2)
!$OMP END PARALLEL

!$OMP PARALLEL &
!
!$OMP & NUM_THREADS(2)
!$OMP END PARALLEL


!$OMP PARALLEL &		! { dg-error "Unclassifiable OpenMP" }
!$    NUM_THREADS(2)		! { dg-error "Unclassifiable" }
!$OMP END PARALLEL		! { dg-error "Unexpected" }

!$OMP PARALLEL &		! { dg-error "Unclassifiable OpenMP" }
!$    & NUM_THREADS(2)		! { dg-error "Unclassifiable" }
!$OMP END PARALLEL		! { dg-error "Unexpected" }

!$OMP PARALLEL &		! { dg-error "Unclassifiable OpenMP" }
!
!$    NUM_THREADS(2)		! { dg-error "Unclassifiable" }
!$OMP END PARALLEL		! { dg-error "Unexpected" }

!$OMP PARALLEL &		! { dg-error "Unclassifiable OpenMP" }
!
!$    & NUM_THREADS(2)		! { dg-error "Unclassifiable" }
!$OMP END PARALLEL		! { dg-error "Unexpected" }

END PROGRAM
