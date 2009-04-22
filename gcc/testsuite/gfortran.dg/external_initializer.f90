! { dg-do compile }
! PR20849 - An external symbol may not have a initializer.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
REAL, EXTERNAL :: X=0 ! { dg-error "not have an initializer" }
END
