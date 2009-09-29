! { dg-do compile }
! { dg-options "-std=legacy" }
! We want to check for statement functions, thus legacy mode.

! Check for errors with declarations not allowed within BLOCK.

SUBROUTINE proc (a)
  IMPLICIT NONE
  INTEGER :: a

  BLOCK
    INTENT(IN) :: a ! { dg-error "not allowed inside of BLOCK" }
    VALUE :: a ! { dg-error "not allowed inside of BLOCK" }
    OPTIONAL :: a ! { dg-error "not allowed inside of BLOCK" }
  END BLOCK
END SUBROUTINE proc

PROGRAM main
  IMPLICIT NONE

  BLOCK 
    IMPLICIT INTEGER(a-z) ! { dg-error "not allowed inside of BLOCK" }
    INTEGER :: a, b, c, d
    INTEGER :: stfunc
    stfunc(a, b) = a + b ! { dg-error "not allowed inside of BLOCK" }
    EQUIVALENCE (a, b) ! { dg-error "not allowed inside of BLOCK" }
    NAMELIST /NLIST/ a, b ! { dg-error "not allowed inside of BLOCK" }
    COMMON /CBLOCK/ c, d ! { dg-error "not allowed inside of BLOCK" }
  ! This contains is in the specification part.
  CONTAINS ! { dg-error "Unexpected CONTAINS statement" }
  END BLOCK

  BLOCK
    PRINT *, "Hello, world"
  ! This one in the executable statement part.
  CONTAINS ! { dg-error "Unexpected CONTAINS statement" }
  END BLOCK
END PROGRAM main
