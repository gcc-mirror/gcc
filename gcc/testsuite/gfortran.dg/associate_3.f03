! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/38936
! Check for errors with ASSOCIATE during parsing.

PROGRAM main
  IMPLICIT NONE

  ASSOCIATE ! { dg-error "Expected association list" }

  ASSOCIATE () ! { dg-error "Expected association" }

  ASSOCIATE (a => 1) 5 ! { dg-error "Junk after ASSOCIATE" }

  ASSOCIATE (x =>) ! { dg-error "Invalid association target" }

  ASSOCIATE (=> 5) ! { dg-error "Expected association" }

  ASSOCIATE (x => 5, ) ! { dg-error "Expected association" }

  myname: ASSOCIATE (a => 1)
  END ASSOCIATE ! { dg-error "Expected block name of 'myname'" }

  ASSOCIATE (b => 2)
  END ASSOCIATE myname ! { dg-error "Syntax error in END ASSOCIATE" }

  myname2: ASSOCIATE (c => 3)
  END ASSOCIATE myname3 ! { dg-error "Expected label 'myname2'" }

  ASSOCIATE (a => 1, b => 2, a => 3) ! { dg-error "Duplicate name 'a'" }

  ASSOCIATE (a => 5)
    INTEGER :: b ! { dg-error "Unexpected data declaration statement" }
  END ASSOCIATE
END PROGRAM main ! { dg-error "Expecting END ASSOCIATE" }
! { dg-excess-errors "Unexpected end of file" }
