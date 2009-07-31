! { dg-do compile }
! { dg-options "-O2 -fdump-tree-lim" }
! gfortran -c -m32 -O2 -S junk.f
!
      MODULE LES3D_DATA

      IMPLICIT REAL*8 (A-H,O-Z)

      PARAMETER ( NSPECI = 1, ND = 7 + NSPECI )

      INTEGER IMAX

      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) ::
     >         UAV,QAV


      END MODULE LES3D_DATA
!---------------------------------------------------------------------
!------------------------------------------------------------------------
      SUBROUTINE FLUXI()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE QS(:)

      ALLOCATE( QS(0:IMAX))
      QS=0D0

      RETURN
      END
!------------------------------------------------------------------------
!------------------------------------------------------------------------
      SUBROUTINE EXTRAPI()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      I1 = 0
      I2 = IMAX - 1

            DO I = I1, I2
               UAV(I,1,2) = QAV(I,1,2)
            END DO

      RETURN
      END
! { dg-final { scan-tree-dump-times "stride" 4 "lim1" } }
! { dg-final { cleanup-tree-dump "lim\[1-2\]" } }
! { dg-final { cleanup-modules "LES3D_DATA" } }
