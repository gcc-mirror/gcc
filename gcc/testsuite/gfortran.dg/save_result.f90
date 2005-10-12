! { dg-do compile }
! PR20856 - A function result may not have SAVE attribute.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
FUNCTION X() RESULT(Y)
REAL, SAVE :: Y ! { dg-error "RESULT attribute conflicts with SAVE" }
y = 1
END FUNCTION X
END
