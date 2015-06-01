! { dg-additional-options "-fprofile-generate" }
  SUBROUTINE matmul_test ( ntim,len)
    INTEGER, PARAMETER :: dp=8
    REAL(KIND=dp), ALLOCATABLE, DIMENSION(:, :) :: ma, mb, mc
    INTEGER :: siz,len, ntim
    DO i = 5, siz, 2
       len = 2**i + 1
       ALLOCATE ( ma ( len, len ), STAT = ierr )
       IF ( ierr /= 0 ) EXIT
       ALLOCATE ( mb ( len, len ), STAT = ierr )
       IF ( ierr /= 0 ) EXIT
       ALLOCATE ( mc ( len, len ), STAT = ierr )
       IF ( ierr /= 0 ) EXIT
       DO j = 1, ntim
          mc = MATMUL ( ma, mb )
       END DO
    END DO
  END SUBROUTINE matmul_test
