! { dg-do run }
! { dg-additional-sources set_vm_limit.c }

USE :: ISO_C_BINDING !, only: C_INT
IMPLICIT NONE

INTERFACE
  SUBROUTINE set_vm_limit(n) bind(C)
  import
  integer(C_INT), value, intent(in) :: n
  END SUBROUTINE set_vm_limit
END INTERFACE

TYPE foo
  INTEGER, DIMENSION(10000) :: data = 42
END TYPE
TYPE(foo), POINTER :: foo_ptr
TYPE(foo), ALLOCATABLE :: foo_obj
TYPE(foo), ALLOCATABLE, DIMENSION(:) :: foo_array

INTEGER istat

CALL set_vm_limit(1000000)

DO
  ALLOCATE(foo_ptr, stat = istat)
  IF (istat .NE. 0) THEN
    PRINT *, "foo_ptr allocation failed"
    EXIT
  ENDIF
ENDDO

ALLOCATE(foo_obj, stat = istat)
IF (istat .NE. 0) THEN
  PRINT *, "foo_obj allocation failed"
ENDIF

ALLOCATE(foo_array(5), stat = istat)
IF (istat .NE. 0) THEN
  PRINT *, "foo_array allocation failed"
ENDIF

END
! { dg-output " *foo_ptr allocation failed(\n|\r\n|\r)" }
! { dg-output " *foo_obj allocation failed(\n|\r\n|\r)" }
! { dg-output " *foo_array allocation failed(\n|\r\n|\r)" }
