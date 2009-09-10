! { dg-do compile }
! { dg-options "-O2 -g" }
SUBROUTINE cp_fm_triangular_multiply()
    INTEGER, PARAMETER :: dp=KIND(0.0D0)
    REAL(dp), ALLOCATABLE, DIMENSION(:)      :: tau, work
    REAL(KIND=dp), DIMENSION(:, :), POINTER  :: a
    ndim = SIZE(a,2)
    ALLOCATE(tau(ndim),STAT=istat)
    ALLOCATE(work(2*ndim),STAT=istat)
END SUBROUTINE
