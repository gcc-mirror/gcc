! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 80121: Memory leak with derived-type intent(out) argument
!
! Contributed by Andrew Wood <andrew@fluidgravity.co.uk>

PROGRAM p
    IMPLICIT NONE
    TYPE t1
      INTEGER, ALLOCATABLE :: i(:)
    END TYPE
    call leak
  CONTAINS
    SUBROUTINE s1(e)
      TYPE(t1), ALLOCATABLE, INTENT(OUT) :: e(:)
      ALLOCATE( e(1) )
      ALLOCATE( e(1)%i(2) )
    END SUBROUTINE
    SUBROUTINE leak
      TYPE(t1), ALLOCATABLE :: e(:)
      CALL s1(e)
      CALL s1(e)
    END SUBROUTINE
END PROGRAM

! { dg-final { scan-tree-dump-times "__builtin_free" 6 "original" } }
