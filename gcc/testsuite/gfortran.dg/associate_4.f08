! { dg-do compile }
! { dg-options "-std=f2008 -fcoarray=single" }

! PR fortran/38936
! Check for error with coindexed target.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: a[*]

  ASSOCIATE (x => a[1]) ! { dg-error "must not be coindexed" }
END PROGRAM main
