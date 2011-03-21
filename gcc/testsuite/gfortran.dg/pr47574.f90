! { dg-do compile }
! PR 47574 - this used to ICE.
      SUBROUTINE EXCH2_UV_AGRID_3D_RL( uPhi, vPhi, myNz )

      IMPLICIT NONE

      INTEGER, parameter :: sNx=32, sNy=32, OLx=4, OLy=4

      INTEGER myNz
      Real(8) uPhi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,3,1)
      REAL(8) vPhi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,3,1)

      INTEGER i,j,k,bi,bj
      REAL(8) uLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      REAL(8) vLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      REAL(8) negOne

      negOne = 1.
        DO k = 1,myNz
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           uLoc(i,j) = uPhi(i,j,k,bi,bj)
           vLoc(i,j) = vPhi(i,j,k,bi,bj)
          ENDDO
         ENDDO
         DO j = 1-OLy,sNy+OLy
          DO i = 1,OLx
           uPhi(1-i,j,k,bi,bj) = vLoc(1-i,j)
           vPhi(1-i,j,k,bi,bj) = uLoc(1-i,j)*negOne
          ENDDO
         ENDDO

        ENDDO

      END

