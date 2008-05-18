! { dg-do compile }
! { dg-options "-fmax-errors=1000" }

  character(kind=1,len=20) :: s1, t1
  character(kind=4,len=20) :: s4, t4

  print *, "" // ""
  print *, "" // 4_"" ! { dg-error "Operands of string concatenation operator" }
  print *, 4_"" // "" ! { dg-error "Operands of string concatenation operator" }
  print *, 4_"" // 4_""

  print *, s1 // ""
  print *, s1 // 4_"" ! { dg-error "Operands of string concatenation operator" }
  print *, s4 // "" ! { dg-error "Operands of string concatenation operator" }
  print *, s4 // 4_""

  print *, "" // s1
  print *, 4_"" // s1 ! { dg-error "Operands of string concatenation operator" }
  print *, "" // s4 ! { dg-error "Operands of string concatenation operator" }
  print *, 4_"" // s4

  print *, s1 // t1
  print *, s1 // t4 ! { dg-error "Operands of string concatenation operator" }
  print *, s4 // t1 ! { dg-error "Operands of string concatenation operator" }
  print *, s4 // t4

  print *, s1 .eq. ""
  print *, s1 .eq. 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 .eq. "" ! { dg-error "Operands of comparison operator" }
  print *, s4 .eq. 4_""

  print *, s1 == ""
  print *, s1 == 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 == "" ! { dg-error "Operands of comparison operator" }
  print *, s4 == 4_""

  print *, s1 .ne. ""
  print *, s1 .ne. 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 .ne. "" ! { dg-error "Operands of comparison operator" }
  print *, s4 .ne. 4_""

  print *, s1 /= ""
  print *, s1 /= 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 /= "" ! { dg-error "Operands of comparison operator" }
  print *, s4 /= 4_""

  print *, s1 .le. ""
  print *, s1 .le. 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 .le. "" ! { dg-error "Operands of comparison operator" }
  print *, s4 .le. 4_""

  print *, s1 <= ""
  print *, s1 <= 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 <= "" ! { dg-error "Operands of comparison operator" }
  print *, s4 <= 4_""

  print *, s1 .ge. ""
  print *, s1 .ge. 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 .ge. "" ! { dg-error "Operands of comparison operator" }
  print *, s4 .ge. 4_""

  print *, s1 >= ""
  print *, s1 >= 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 >= "" ! { dg-error "Operands of comparison operator" }
  print *, s4 >= 4_""

  print *, s1 .lt. ""
  print *, s1 .lt. 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 .lt. "" ! { dg-error "Operands of comparison operator" }
  print *, s4 .lt. 4_""

  print *, s1 < ""
  print *, s1 < 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 < "" ! { dg-error "Operands of comparison operator" }
  print *, s4 < 4_""

  print *, s1 .gt. ""
  print *, s1 .gt. 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 .gt. "" ! { dg-error "Operands of comparison operator" }
  print *, s4 .gt. 4_""

  print *, s1 > ""
  print *, s1 > 4_"" ! { dg-error "Operands of comparison operator" }
  print *, s4 > "" ! { dg-error "Operands of comparison operator" }
  print *, s4 > 4_""

  print *, "" ==  ""
  print *, 4_"" ==  "" ! { dg-error "Operands of comparison operator" }
  print *, "" .eq.  ""
  print *, 4_"" .eq.  "" ! { dg-error "Operands of comparison operator" }
  print *, "" /=  ""
  print *, 4_"" /=  "" ! { dg-error "Operands of comparison operator" }
  print *, "" .ne.  ""
  print *, 4_"" .ne.  "" ! { dg-error "Operands of comparison operator" }
  print *, "" .lt.  ""
  print *, 4_"" .lt.  "" ! { dg-error "Operands of comparison operator" }
  print *, "" <  ""
  print *, 4_"" <  "" ! { dg-error "Operands of comparison operator" }
  print *, "" .le.  ""
  print *, 4_"" .le.  "" ! { dg-error "Operands of comparison operator" }
  print *, "" <=  ""
  print *, 4_"" <=  "" ! { dg-error "Operands of comparison operator" }
  print *, "" .gt.  ""
  print *, 4_"" .gt.  "" ! { dg-error "Operands of comparison operator" }
  print *, "" >  ""
  print *, 4_"" >  "" ! { dg-error "Operands of comparison operator" }
  print *, "" .ge.  ""
  print *, 4_"" .ge.  "" ! { dg-error "Operands of comparison operator" }
  print *, "" >=  ""
  print *, 4_"" >=  "" ! { dg-error "Operands of comparison operator" }

  end
