! { dg-do compile }
! { dg-options "-std=gnu -Wsurprising" }

! PR fortran/30239
! Check for errors when a symbol gets declared a type twice, even if it
! is the same.

INTEGER FUNCTION foo ()
  IMPLICIT NONE
  INTEGER :: foo ! { dg-error "basic type of" }
  INTEGER :: foo ! { dg-error "basic type of" }
  foo = 42
END FUNCTION foo

INTEGER FUNCTION bar () RESULT (x)
  IMPLICIT NONE
  INTEGER :: x ! { dg-error "basic type of" }

  INTEGER :: y
  INTEGER :: y ! { dg-error "basic type of" }

  x = 42
END FUNCTION bar
