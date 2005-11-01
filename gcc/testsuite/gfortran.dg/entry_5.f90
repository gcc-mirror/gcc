! { dg-do compile }
! PR 24008
! an argument list to the entry is required
REAL FUNCTION funct()
  funct = 0.0
  RETURN
!
  ENTRY enter RESULT (answer)  ! { dg-error "Unclassifiable statement" }
  answer = 1.0
  RETURN
END FUNCTION funct
