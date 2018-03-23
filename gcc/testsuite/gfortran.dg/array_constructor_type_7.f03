! { dg-do run }
! { dg-options "-fbounds-check" }
!
! PR fortran/27997
!
! Array constructor with typespec and dynamic
! character length.
!
PROGRAM test
  CALL foo(8, "short", "test", "short")
  CALL foo(2, "lenghty", "te", "le")
CONTAINS
  SUBROUTINE foo (n, s, a1, a2)
    CHARACTER(len=*) :: s
    CHARACTER(len=*) :: a1, a2
    CHARACTER(len=n) :: arr(2)
    INTEGER :: n
    arr = [ character(len=n) :: 'test', s ]
    IF (arr(1) /= a1 .OR. arr(2) /= a2) THEN
      STOP 1
    END IF
  END SUBROUTINE foo
END PROGRAM test
