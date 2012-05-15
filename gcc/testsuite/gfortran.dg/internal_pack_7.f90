! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR43072, in which unnecessary calls to
! internal PACK/UNPACK were being generated.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE M1
  PRIVATE
  REAL, PARAMETER :: c(2)=(/(i,i=1,2)/)
CONTAINS
  ! WAS OK
  SUBROUTINE S0
    real :: r
     r=0
     r=S2(c)
     r=S2((/(real(i),i=1,2)/)) ! See comment #1 of the PR
  END SUBROUTINE S0
  ! WAS NOT OK
  SUBROUTINE S1
    real :: r
     r=0
     r=r+S2(c)
     r=r+S2((/(real(i),i=1,2)/)) ! See comment #1 of the PR
  END SUBROUTINE S1

  FUNCTION S2(c)
     REAL, INTENT(IN) :: c(2)
     s2=0
  END FUNCTION S2
END MODULE M1
! { dg-final { scan-tree-dump-times "pack" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
