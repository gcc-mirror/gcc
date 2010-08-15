! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/38936
! Check for errors with ASSOCIATE during resolution.

PROGRAM main
  IMPLICIT NONE

  ASSOCIATE (a => 5) ! { dg-error "is used as array" }
    PRINT *, a(3)
  END ASSOCIATE
END PROGRAM main
