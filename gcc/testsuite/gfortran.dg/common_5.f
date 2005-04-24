C { dg-do compile }
C PR 20059
C Check that the warning for padding works correctly.
      SUBROUTINE PLOTZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /CCPOOL/ RMIN,RMAX,ZMIN,ZMAX,IMIN,JMIN,IMAX,JMAX,NFLOP, ! { dg-warning "Padding" }
     $ HTP
C
        RETURN
      END
