! { dg-do compile }
! { dg-options "-fdec-static" }
! An AUTOMATIC statement cannot duplicated
FUNCTION X()
REAL, AUTOMATIC, AUTOMATIC :: Y ! { dg-error "Duplicate AUTOMATIC attribute" }
y = 1
END FUNCTION X
END
