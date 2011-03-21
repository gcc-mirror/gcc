! { dg-do compile }
! { dg-options "-c" }
!
! Do not apply the SAVE attribute to function results.
!
FUNCTION f() RESULT (g)
  INTEGER :: g
  SAVE
  g = 42
END FUNCTION
