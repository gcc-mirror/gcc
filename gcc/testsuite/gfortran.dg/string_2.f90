! { dg-do compile }
!
program main
  implicit none
  character(len=10) :: s

  s = ''
  print *, s(1:2_8**32_8+3_8) ! { dg-error "exceeds the string length" }
  print *, s(2_8**32_8+3_8:2_8**32_8+4_8) ! { dg-error "exceeds the string length" }
  print *, len(s(1:2_8**32_8+3_8)) ! { dg-error "exceeds the string length" }

end program main
