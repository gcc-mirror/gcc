! { dg-do compile }
! PR 46020 - check for clear error message
! { dg-options "" }
FUNCTION F_X(A) bind(c,name='F_X') ! { dg-error "must have length 1" }
   CHARACTER*(*) F_X
END FUNCTION


FUNCTION F_Y(A) bind(c,name='F_Y') ! { dg-error "must have length 1" }
   CHARACTER*(2) F_Y
END FUNCTION


