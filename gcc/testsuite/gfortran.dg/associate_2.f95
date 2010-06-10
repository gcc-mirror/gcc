! { dg-do compile }
! { dg-options "-std=f95" }

! PR fortran/38936
! Test that F95 rejects ASSOCIATE.

PROGRAM main
  IMPLICIT NONE

  ASSOCIATE (a => 5) ! { dg-error "Fortran 2003" }
  END ASSOCIATE
END PROGRAM main
