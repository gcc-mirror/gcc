! { dg-do run }
! PR117819 
Program xe1
  Implicit None
  Character(6) string
  Integer x
  Logical :: ok = .True.
  string = '111111'
  !print *, "String we read from is: ", string
  Read(string,1) x
1 Format(BZ,B8)
  If (x/=Int(b'11111100')) Then
    Print *,'FAIL B8 BZ wrong result'
    Print *,'Expected',Int(b'11111100')
    Print *,'Received',x
    ok = .False.
  End If
  string = '123456'
  !print *, "String we read from is: ", string
  Read(string,2) x
2 Format(BZ,I8)
  If (x/=12345600) Then
    Print *,'FAIL I8 BZ wrong result'
    Print *,'Expected',12345600
    Print *,'Received',x
    ok = .False.
  End If
  Read(string,3) x
3 Format(BZ,O8)
  If (x/=Int(o'12345600')) Then
    Print *,'FAIL O8 BZ wrong result'
    Print *,'Expected',Int(o'12345600')
    Print *,'Received',x
    ok = .False.
  End If
  Read(string,4) x
4 Format(BZ,Z8)
  If (x/=Int(z'12345600')) Then
    Print *,'FAIL OZ BZ wrong result'
    Print *,'Expected',Int(z'12345600')
    Print *,'Received',x
    ok = .False.
  End If
  If (.not. ok) stop 1
End Program
