! { dg-do run }
!
! Test the fix for PR86760 in which temporaries were not being
! assigned for array component references.
!
! Contributed by Chris Hansen  <hansec@uw.edu>
!
MODULE test_nesting_mod
  IMPLICIT NONE
  TYPE :: test_obj1
  CONTAINS
    PROCEDURE :: destroy
  END TYPE

  TYPE :: obj_ptr
    CLASS(test_obj1), POINTER :: f => NULL()
  END TYPE

  TYPE :: obj_container
    TYPE(obj_ptr), POINTER, DIMENSION(:) :: v => NULL()
  END TYPE

  integer :: ctr = 0

CONTAINS

  SUBROUTINE destroy(self)
    CLASS(test_obj1), INTENT(INOUT):: self
    ctr = ctr + 1
  END SUBROUTINE

  SUBROUTINE container_destroy(self)
    type(obj_container), INTENT(INOUT) :: self
    INTEGER :: i
    DO i=1,ubound(self%v,1)
      CALL self%v(i)%f%destroy()
    END DO
  END SUBROUTINE

END MODULE


PROGRAM test_nesting_ptr
  USE test_nesting_mod
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, PARAMETER :: n = 2
  TYPE(obj_container) :: var

  ALLOCATE(var%v(n))
  DO i=1,n
    ALLOCATE(test_obj1::var%v(i)%f)
  END DO
  CALL container_destroy(var)

  if (ctr .ne. 2) stop 1
END
