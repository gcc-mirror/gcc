! { dg-do compile }
! Tests the fix for PR36366 a regression in which the order of USE statements
! in 'test2' would cause the result of 'test1' not to have a reference to
! the derived type 'inner'.
!
! Contributed by Jakub Jelinek <jakub@gcc.gnu.org>
!
MODULE types
  IMPLICIT NONE
  TYPE :: inner
    INTEGER, POINTER :: i(:)
  END TYPE inner

  TYPE :: outer
    TYPE(inner), POINTER :: inr(:)
  END TYPE outer
END MODULE types

MODULE mymod
  IMPLICIT NONE
CONTAINS
  FUNCTION test1()
    USE types
    IMPLICIT NONE
    TYPE(outer), POINTER :: test1
    NULLIFY(test1)
  END FUNCTION test1
END MODULE mymod

MODULE test
  IMPLICIT NONE
CONTAINS

  SUBROUTINE test2(a)
    USE mymod
    USE types
    IMPLICIT NONE
    TYPE(outer), INTENT(INOUT) :: a
    INTEGER :: i
    i = a%inr(1)%i(1)
  END SUBROUTINE test2

  SUBROUTINE test3(a)
    USE types
    IMPLICIT NONE
    TYPE(outer), INTENT(IN) :: a
  END SUBROUTINE test3
END MODULE test
! { dg-final { cleanup-modules "types mymod test" } }
