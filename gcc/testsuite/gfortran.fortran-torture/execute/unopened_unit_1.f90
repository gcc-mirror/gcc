! PR 14565
program unopened_unit_1
  Integer I,J
  Do I = 1,10
    Write(99,*)I
  End Do
  Rewind(99)
  Do I = 1,10
    Read(99,*)J
    If (J.ne.I) STOP 1
  End Do
  Close(99, Status='Delete')
End program

