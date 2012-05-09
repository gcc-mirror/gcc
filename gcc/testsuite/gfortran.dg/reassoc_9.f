! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-optimized" }

      SUBROUTINE S55199(P,Dvdph)
      implicit none
      real(8) :: c1,c2,c3,P,Dvdph
      c1=0.1d0
      c2=0.2d0
      c3=0.3d0
      Dvdph = c1 + 2.*P**2*c2 + 3.*P**4*c3
      END

! There should be three multiplies following un-distribution
! and power expansion.

! { dg-final { scan-tree-dump-times " \\\* " 3 "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
