! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-optimized" }

      SUBROUTINE S55199(P,Q,Dvdph)
      implicit none
      real(8) :: c1,c2,c3,P,Q,Dvdph
      c1=0.1d0
      c2=0.2d0
      c3=0.3d0
      Dvdph = c1 + 2.*P*c2 + 3.*P**2*Q**3*c3
      END

! There should be five multiplies following un-distribution
! and power expansion.

! { dg-final { scan-tree-dump-times " \\\* " 5 "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
