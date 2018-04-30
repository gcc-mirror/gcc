! { dg-do run { target i?86-*-linux* x86_64-*-linux* } }
! { dg-additional-sources set_vm_limit.c }
!
! This test calls set_vm_limit to set an artificially low address space
! limit.  set_vm_limit calls setrlimit, which has some portability
! considerations.  setrlimit gets errors on arm*linux and aarch64*linux,
! and when the main program calls malloc(), it in turn fails on Darwin.
! The code being tested is portable, calling ALLOCATED() or ASSOCIATED()
! to verify that allocation was successful, so the operating assumption
! is that as long as this test runs on at least one system, we can call
! it good.

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
