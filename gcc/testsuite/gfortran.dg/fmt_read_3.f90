! { dg-do compile }
! PR52392 "READ format" statement with parenthesed default-char-expr
PROGRAM ReadMeTwo
  IMPLICIT NONE
  CHARACTER(10) :: var
  var = "TestStr"
  PRINT ('(') // 'A)', var 
  PRINT ('(') // 'A)', var 
  READ ('(') // 'A)', var  
  PRINT *, var
  READ *, var
END PROGRAM ReadMeTwo

