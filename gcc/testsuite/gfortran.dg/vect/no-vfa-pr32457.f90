! { dg-do compile }
! { dg-require-effective-target vect_float }

SUBROUTINE KEEL(RBOUND)
  REAL, DIMENSION(0:100) :: RBOUND
  DO N = 1, NP1
     RBOUND(N) = RBOUND(N-1) + 1
  END DO
  DO N = 1, NS
     WRITE (16,'(I5)') SRAD(N)
  END DO
END SUBROUTINE KEEL

! { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } }
! { dg-final { cleanup-tree-dump "vect" } }
