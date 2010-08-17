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
    PRINT *, a(3)
  END ASSOCIATE

  ASSOCIATE (a => nontarget)
    ptr => a ! { dg-error "neither TARGET nor POINTER" }
  END ASSOCIATE

  ASSOCIATE (a => 5, & ! { dg-error "variable definition context" }
             b => arr((/ 1, 3 /))) ! { dg-error "variable definition context" }
    a = 4
    b = 7
  END ASSOCIATE
END PROGRAM main
