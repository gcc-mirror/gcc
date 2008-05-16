! { dg-do run }
! { dg-options "-fbounds-check" }
!
! PR fortran/27997
!
! Array constructor with typespec and dynamic
! character length.
!
PROGRAM test
  CALL foo(8, "short", "short")
  CALL foo(2, "lenghty", "le")
CONTAINS
  SUBROUTINE foo (n, s, shouldBe)
    CHARACTER(len=*) :: s
    CHARACTER(len=*) :: shouldBe
    CHARACTER(len=16) :: arr(2)
    INTEGER :: n
    arr = [ character(len=n) :: s, s ]
    IF (arr(1) /= shouldBe .OR. arr(2) /= shouldBe) THEN
      CALL abort ()
    END IF
  END SUBROUTINE foo
END PROGRAM test
