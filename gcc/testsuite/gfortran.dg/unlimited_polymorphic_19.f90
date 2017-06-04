! { dg-do run }
!
! PR 64209: [OOP] runtime segfault with CLASS(*), INTENT(OUT) dummy argument
!
! Contributed by Miha Polajnar <polajnar.miha@gmail.com>

MODULE m
  IMPLICIT NONE
  TYPE :: t
    CLASS(*), ALLOCATABLE :: x(:)
  CONTAINS
    PROCEDURE :: copy
  END TYPE t
  INTERFACE 
    SUBROUTINE copy_proc_intr(a,b)
      CLASS(*), INTENT(IN) :: a
      CLASS(*), INTENT(OUT) :: b
    END SUBROUTINE copy_proc_intr
  END INTERFACE 
CONTAINS
  SUBROUTINE copy(self,cp,a)
    CLASS(t), INTENT(IN) :: self
    PROCEDURE(copy_proc_intr) :: cp
    CLASS(*), INTENT(OUT) :: a(:)
    INTEGER :: i
    IF( .not.same_type_as(self%x(1),a(1)) ) STOP -1
    DO i = 1, size(self%x)
      CALL cp(self%x(i),a(i))
    END DO
  END SUBROUTINE copy
END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE
  INTEGER, PARAMETER :: n = 3, x(n) = [ 1, 2, 3 ] 
  INTEGER :: copy_x(n)
  TYPE(t) :: test
  ALLOCATE(test%x(n),SOURCE=x)
  CALL test%copy(copy_int,copy_x)
!   PRINT '(*(I0,:2X))', copy_x
CONTAINS
  SUBROUTINE copy_int(a,b)
    CLASS(*), INTENT(IN) :: a
    CLASS(*), INTENT(OUT) :: b
    SELECT TYPE(a); TYPE IS(integer) 
    SELECT TYPE(b); TYPE IS(integer)
      b = a
    END SELECT; END SELECT
  END SUBROUTINE copy_int 
END PROGRAM main
