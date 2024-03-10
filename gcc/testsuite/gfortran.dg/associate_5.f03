! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/38936
! Check for errors with ASSOCIATE during resolution.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: nontarget
  INTEGER :: arr(3)
  INTEGER, POINTER :: ptr

  ASSOCIATE (a => 5) ! { dg-error "is used as array" }
    PRINT *, a(3) ! { dg-error "has an array reference" }
  END ASSOCIATE

  ASSOCIATE (a => nontarget)
    ptr => a ! { dg-error "neither TARGET nor POINTER" }
  END ASSOCIATE

  ASSOCIATE (a => 5, b => arr((/ 1, 3 /)))
    a = 4 ! { dg-error "variable definition context" }
    b = 7 ! { dg-error "variable definition context" }
    CALL test2 (a) ! { dg-error "variable definition context" }
    CALL test2 (b) ! { dg-error "variable definition context" }
  END ASSOCIATE

CONTAINS

  SUBROUTINE test (x)
    INTEGER, INTENT(IN) :: x
    ASSOCIATE (y => x) ! { dg-error "variable definition context" }
      y = 5 ! { dg-error "variable definition context" }
      CALL test2 (x) ! { dg-error "variable definition context" }
    END ASSOCIATE
  END SUBROUTINE test

  ELEMENTAL SUBROUTINE test2 (x)
    INTEGER, INTENT(OUT) :: x
    x = 5
  END SUBROUTINE test2

END PROGRAM main
