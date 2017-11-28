! PR fortran/82568

MODULE PR82568_MOD
  INTEGER :: N
END MODULE
PROGRAM PR82568
  INTEGER :: I, L
  !$OMP PARALLEL DO
  DO I=1,2
    BLOCK
      USE PR82568_MOD
      INTEGER :: J
      DO J=1,2
        PRINT*,I,J
      END DO
      DO K=1,2
        PRINT*,I,K
      END DO
      DO L=1,2
        PRINT*,I,L
      END DO
      DO N=1,2
        PRINT*,I,N
      END DO
    END BLOCK
    DO M=1,2
      PRINT*,I,M
    END DO
  END DO
  !$OMP TASK
  DO I=1,2
    BLOCK
      USE PR82568_MOD
      INTEGER :: J
      DO J=1,2
        PRINT*,I,J
      END DO
      DO K=1,2
        PRINT*,I,K
      END DO
      DO L=1,2
        PRINT*,I,L
      END DO
      DO N=1,2
        PRINT*,I,N
      END DO
    END BLOCK
    DO M=1,2
      PRINT*,I,M
    END DO
  END DO
  !$OMP END TASK
  !$OMP TASKLOOP
  DO I=1,2
    BLOCK
      USE PR82568_MOD
      INTEGER :: J
      DO J=1,2
        PRINT*,I,J
      END DO
      DO K=1,2
        PRINT*,I,K
      END DO
      DO L=1,2
        PRINT*,I,L
      END DO
      DO N=1,2
        PRINT*,I,N
      END DO
    END BLOCK
    DO M=1,2
      PRINT*,I,M
    END DO
  END DO
END PROGRAM PR82568
