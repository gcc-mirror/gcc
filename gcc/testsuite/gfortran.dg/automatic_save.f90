! { dg-do compile }
! { dg-options "-fdec-static" }
! An AUTOMATIC statement cannot be used with SAVE
FUNCTION X()
REAL, SAVE, AUTOMATIC :: Y ! { dg-error "AUTOMATIC attribute conflicts with SAVE attribute" }
y = 1
END FUNCTION X
END
