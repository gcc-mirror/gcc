! { dg-do run { target *-*-cygwin* *-*-mingw* } }
! { dg-options "-lkernel32" }
! Test case provided by Dennis Wassel.

PROGRAM winapi

  USE, INTRINSIC :: iso_c_binding
  IMPLICIT NONE

  INTERFACE
     ! Specifically select the lstrlenA version for ASCII.
     FUNCTION lstrlen(string) BIND(C, name = "lstrlenA")
       USE, INTRINSIC :: iso_c_binding
       IMPLICIT NONE
       !GCC$ ATTRIBUTES STDCALL :: lstrlen
       INTEGER (C_INT)          :: lstrlen
       CHARACTER(KIND=C_CHAR), INTENT(in) :: string(*)
     END FUNCTION lstrlen
  END INTERFACE
  
  IF (lstrlen(C_CHAR_"winapi"//C_NULL_CHAR) /= 6) STOP 1

END PROGRAM winapi
