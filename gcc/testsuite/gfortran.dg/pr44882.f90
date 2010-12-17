! { dg-do compile }
! { dg-options "-O3 -ffast-math -funroll-loops -w" }

      SUBROUTINE TRUDGE(KDIR)
! There is a type mismatch here for TRUPAR which caused an ICE
      COMMON /TRUPAR/ DR(10),V(10,10)
      DO 110 I=1,NDIR
  110 DR(I)=V(I,JDIR)
      END
      SUBROUTINE TRUSRC(LEAVE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /TRUPAR/ DX(10),V(10,10)
      END

