      MODULE LES3D_DATA
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) ::
     >             P, T, H
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:) ::
     >             HF
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::
     >             Q
      END MODULE LES3D_DATA
      USE LES3D_DATA
      DO K = 1, KMAX - 1
         DO J = 1, JMAX - 1
            DO I = 1, I2
               T(I,J,K) = (EI - HF(I,J,K,1)) / HF(I,J,K,3)
            ENDDO
            P(1:I2,J,K) = Q(1:I2,J,K,1,M) * HF(1:I2,J,K,4) * T(1:I2,J,K)
            IF(ISGSK .EQ. 1) H(1:I2,J,K) =
     >                   (Q(1:I2,J,K,5,M) + P(1:I2,J,K))
         END DO
      ENDDO
      END

! { dg-final { cleanup-modules "les3d_data" } }
