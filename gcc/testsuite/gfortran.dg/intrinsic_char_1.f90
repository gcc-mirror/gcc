! { dg-do run }
! Tests the fix for PR35932, in which the KIND argument of CHAR
! was not converted and this screwed up the scalarizer.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
program FA0005

  CHARACTER(1) CDA1(10)
  character(10) CDA10
  INTEGER :: IDA(10) = [(i, i = 97,106)]

  CDA1 = CHAR (  IDA, KIND("A" ))     !failed
  if (transfer (CDA1, CDA10) /= "abcdefghij") STOP 1
  CDA1 = CHAR (  IDA  )               !worked
  if (transfer (CDA1, CDA10) /= "abcdefghij") STOP 2
END 
