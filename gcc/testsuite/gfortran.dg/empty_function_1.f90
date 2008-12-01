! { dg-do compile }
!
! PR fortran/38252
! FUNCTION rejected if both specification and execution part are empty
!
! Contributed by Daniel Kraft <d@domob.eu>

INTEGER FUNCTION test ()
CONTAINS
END FUNCTION test
