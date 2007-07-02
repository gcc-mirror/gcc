! { dg-do run }
! { dg-options "-O2 -ftree-vectorize -ffast-math" }
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
SUBROUTINE T(nsubcell,sab_max,subcells)
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
  REAL(dp) :: sab_max(3), subcells,nsubcell(3)
  nsubcell(:) = MIN(MAX(1,NINT(0.5_dp*subcells/sab_max(:))),20)
END SUBROUTINE T

INTEGER, PARAMETER :: dp=KIND(0.0D0)
REAL(dp) :: sab_max(3), subcells,nsubcell(3)
subcells=2.0_dp
sab_max=0.590060749244805_dp
CALL T(nsubcell,sab_max,subcells)
IF (ANY(nsubcell.NE.2.0_dp)) CALL ABORT()
END
