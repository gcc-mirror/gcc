! { dg-do compile }
! { dg-options "-Wimplicit-procedure" }

! PR fortran/22552
! Check for correct -Wimplicit-procedure warnings.

MODULE m

CONTAINS

  SUBROUTINE my_sub ()
  END SUBROUTINE my_sub

  INTEGER FUNCTION my_func ()
    my_func = 42
  END FUNCTION my_func

END MODULE m

SUBROUTINE test (proc)
  IMPLICIT NONE
  CALL proc () ! { dg-bogus "is not explicitly declared" }
END SUBROUTINE test

PROGRAM main
  USE m
  EXTERNAL :: ext_sub
  EXTERNAL :: test
  INTEGER :: ext_func

  CALL ext_sub () ! { dg-bogus "is not explicitly declared" }
  PRINT *, ext_func () ! { dg-bogus "is not explicitly declared" }
  PRINT *, implicit_func () ! { dg-bogus "is not explicitly declared" }
  CALL my_sub () ! { dg-bogus "is not explicitly declared" }
  PRINT *, my_func () ! { dg-bogus "is not explicitly declared" }
  PRINT *, SIN (3.14159) ! { dg-bogus "is not explicitly declared" }

  CALL undef_sub (1, 2, 3) ! { dg-warning "is not explicitly declared" }
  ! Can't check undefined function, because it needs to be declared a type
  ! in any case (and the implicit type is enough to not trigger this warning).
END PROGRAM
