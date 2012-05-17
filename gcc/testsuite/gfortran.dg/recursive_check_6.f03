! { dg-do compile }

! PR fortran/37779
! Check that a call to a procedure's containing procedure counts as recursive
! and is rejected if the containing procedure is not RECURSIVE.

MODULE m
  IMPLICIT NONE

CONTAINS

  SUBROUTINE test_sub ()
    CALL bar ()
  CONTAINS
    SUBROUTINE bar ()
      IMPLICIT NONE
      PROCEDURE(test_sub), POINTER :: procptr

      CALL test_sub () ! { dg-error "not RECURSIVE" }
      procptr => test_sub ! { dg-warning "Non-RECURSIVE" }
      CALL foobar (test_sub) ! { dg-warning "Non-RECURSIVE" }
    END SUBROUTINE bar
  END SUBROUTINE test_sub

  INTEGER FUNCTION test_func () RESULT (x)
    x = bar ()
  CONTAINS
    INTEGER FUNCTION bar ()
      IMPLICIT NONE
      PROCEDURE(test_func), POINTER :: procptr

      bar = test_func () ! { dg-error "not RECURSIVE" }
      procptr => test_func ! { dg-warning "Non-RECURSIVE" }
      CALL foobar (test_func) ! { dg-warning "Non-RECURSIVE" }
    END FUNCTION bar
  END FUNCTION test_func

  SUBROUTINE sub_entries ()
  ENTRY sub_entry_1 ()
  ENTRY sub_entry_2 ()
    CALL bar ()
  CONTAINS
    SUBROUTINE bar ()
      CALL sub_entry_1 () ! { dg-error "is not RECURSIVE" }
    END SUBROUTINE bar
  END SUBROUTINE sub_entries

  INTEGER FUNCTION func_entries () RESULT (x)
  ENTRY func_entry_1 () RESULT (x)
  ENTRY func_entry_2 () RESULT (x)
    x = bar ()
  CONTAINS
    INTEGER FUNCTION bar ()
      bar = func_entry_1 () ! { dg-error "is not RECURSIVE" }
    END FUNCTION bar
  END FUNCTION func_entries

  SUBROUTINE main ()
    CALL test_sub ()
    CALL sub_entries ()
    PRINT *, test_func (), func_entries ()
  END SUBROUTINE main

END MODULE m
