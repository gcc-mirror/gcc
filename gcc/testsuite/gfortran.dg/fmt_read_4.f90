! { dg-do compile }
! PR52393 "READ format" statement with parenthesed default-char-expr
PROGRAM ReadMeTwo
  IMPLICIT NONE
  CHARACTER(10) :: var
  var = "TestStr"
  READ ('((') // 'A)', var ! { dg-error "Unexpected end of format" }
END PROGRAM ReadMeTwo
