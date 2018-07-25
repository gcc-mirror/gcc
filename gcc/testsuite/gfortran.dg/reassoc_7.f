! { dg-do compile }
! { dg-options "-O3 -ffast-math -ffp-contract=off -fdump-tree-optimized" }

      SUBROUTINE S55199(P,Dvdph)
      implicit none
      real(8) :: c1,c2,c3,P,Dvdph
      c1=0.1d0
      c2=0.2d0
      c3=0.3d0
      Dvdph = c1 + 2.*P*c2 + 3.*P**2*c3
      END

! There should be two multiplies following un-distribution.

! { dg-final { scan-tree-dump-times " \\\* " 2 "optimized" } }
