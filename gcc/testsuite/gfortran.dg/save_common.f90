! { dg-do compile }
! PR20847 - A common variable may not have the SAVE attribute.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
INTEGER, SAVE :: X
COMMON /COM/ X ! { dg-error "conflicts with SAVE attribute" }
END
