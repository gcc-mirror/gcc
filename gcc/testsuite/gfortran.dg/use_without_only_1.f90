! PR fortran/61234 Warn for use-stmt without explicit only-list.
! { dg-do compile }
! { dg-options "-Wuse-without-only" }
MODULE foo
  INTEGER :: bar
END MODULE

MODULE testmod
  USE foo ! { dg-warning "6:has no ONLY qualifier" }
  IMPLICIT NONE
CONTAINS
  SUBROUTINE S1
     USE foo ! { dg-warning "9:has no ONLY qualifier" }
  END SUBROUTINE S1
  SUBROUTINE S2
     USE foo, ONLY: bar 
  END SUBROUTINE
  SUBROUTINE S3
     USE ISO_C_BINDING ! { dg-warning "9:has no ONLY qualifier" }
  END SUBROUTINE S3
END MODULE
! { dg-final { cleanup-modules "foo testmod" } }
