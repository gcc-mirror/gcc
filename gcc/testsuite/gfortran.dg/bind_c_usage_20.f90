! { dg-do compile }
! { dg-options "-fcheck=bounds" }
!
! PR fortran/43015
!
! Contributed by Dennis Wassel
!
SUBROUTINE foo(msg) BIND(C, name = "Foo")
  USE, INTRINSIC :: iso_c_binding
  IMPLICIT NONE
  CHARACTER (KIND=C_CHAR), INTENT (out) :: msg(*) 
END SUBROUTINE foo

