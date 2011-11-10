! { dg-do compile }

      SUBROUTINE MLIST(MOLsp,PBCx,PBCy,PBCz, X0)
      IMPLICIT NONE
      INTEGER, PARAMETER :: NM=16384
      INTEGER :: MOLsp, i
      REAL :: PBCx, PBCy, PBCz, boxjmp, HALf=1./2.
      REAL :: X0(2,-2:NM)

         DO i = 1 , MOLsp
            boxjmp = PBCx*INT(X0(1,i)+SIGN(HALf,X0(1,i)))
            X0(1,i) = X0(1,i) - boxjmp
            boxjmp = PBCy*INT(X0(2,i)+SIGN(HALf,X0(2,i)))
            X0(2,i) = X0(2,i) - boxjmp
         ENDDO
      END

! { dg-final { cleanup-tree-dump "vect" } }

