! { dg-do compile }
! { dg-options "-O2 -floop-nest-optimize" }
      MODULE LES3D_DATA
      PARAMETER ( NSCHEME = 4, ICHEM = 0, ISGSK = 0, IVISC = 1 )
      DOUBLE PRECISION DT, TIME, STATTIME, CFL, RELNO, TSTND, ALREF
      INTEGER IDYN, IMAX, JMAX, KMAX
      PARAMETER( RUNIV =  8.3145D3,
     >        TPRANDLT =    0.91D0)
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) ::
     >             U, V, W, P, T, H, EK,
     >         UAV, VAV, WAV, PAV, TAV, HAV, EKAV
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:) ::
     >             CONC, HF, QAV, COAV, HFAV, DU
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::
     >             Q
      END MODULE LES3D_DATA
      SUBROUTINE FLUXJ()
      USE LES3D_DATA
      ALLOCATABLE QS(:), FSJ(:,:,:)
      ALLOCATABLE DWDX(:),DWDY(:),DWDZ(:)
      ALLOCATABLE DHDY(:), DKDY(:)
      PARAMETER (  R12I = 1.0D0 / 12.0D0,
     >             TWO3 = 2.0D0 / 3.0D0 )
      ALLOCATE( QS(IMAX-1), FSJ(IMAX-1,0:JMAX-1,ND))
      ALLOCATE( DWDX(IMAX-1),DWDY(IMAX-1),DWDZ(IMAX-1))
      I1 = 1
      DO K = K1,K2
         DO J = J1,J2
            DO I = I1, I2
               FSJ(I,J,5) = FSJ(I,J,5) + PAV(I,J,K) * QS(I)
            END DO
            DO I = I1, I2
               DWDX(I) = DXI * R12I * (WAV(I-2,J,K) - WAV(I+2,J,K) +
     >                        8.0D0 * (WAV(I+1,J,K) - WAV(I-1,J,K)))
            END DO
         END DO
      END DO
      DEALLOCATE( QS, FSJ, DHDY, DKDY)
      END
