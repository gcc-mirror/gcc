! { dg-do compile }
! { dg-options "-fcheck=bounds" }
!
! PR 42804: ICE with -fcheck=bounds and type bound procedure call on array element
!
! Contributed by Ian Harvey <ian_harvey@bigpond.com>

MODULE ModA
  IMPLICIT NONE
  PRIVATE
  TYPE, PUBLIC :: A
  CONTAINS
    PROCEDURE :: Proc => a_proc
  END TYPE A
CONTAINS
  SUBROUTINE a_proc(this, stat)
    CLASS(A), INTENT(INOUT) :: this
    INTEGER, INTENT(OUT) :: stat
    WRITE (*, *) 'a_proc'
    stat = 0
  END SUBROUTINE a_proc
END MODULE ModA

PROGRAM ProgA
  USE ModA
  IMPLICIT NONE
  INTEGER :: ierr
  INTEGER :: i
  TYPE(A), ALLOCATABLE :: arr(:)
  ALLOCATE(arr(2))
  DO i = 1, 2
    CALL arr(i)%Proc(ierr)
  END DO
END PROGRAM ProgA
 
! { dg-final { cleanup-modules "ModA" } }
