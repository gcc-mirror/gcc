! { dg-do compile }
!
subroutine foo(i)
  implicit none
  integer, intent(in) :: i
  character(len=i) :: s

  s = ''
  print *, s(1:2_8**32_8+3_8) ! { dg-error "too large" }
  print *, s(2_8**32_8+3_8:2_8**32_8+4_8) ! { dg-error "too large" }
  print *, len(s(1:2_8**32_8+3_8)) ! { dg-error "too large" }
  print *, len(s(2_8**32_8+3_8:2_8**32_8+4_8)) ! { dg-error "too large" }

  print *, s(2_8**32_8+3_8:1)
  print *, s(2_8**32_8+4_8:2_8**32_8+3_8)
  print *, len(s(2_8**32_8+3_8:1))
  print *, len(s(2_8**32_8+4_8:2_8**32_8+3_8))

end subroutine
