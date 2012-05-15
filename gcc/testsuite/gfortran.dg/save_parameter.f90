! { dg-do compile }
! PR fortran/32633 - implied SAVE conflicts with parameter attribute
! Testcase contributed by: Joost VandeVondele <jv244@cam.ac.uk>

MODULE test
  CHARACTER(len=1), PARAMETER :: backslash = '\\'
  PUBLIC :: backslash
END MODULE
