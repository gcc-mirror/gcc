! { dg-do compile }
! { dg-require-effective-target lp64 }
! { dg-require-effective-target fortran_integer_16 }
subroutine foo(i)
  implicit none
  integer, intent(in) :: i
  character(len=i) :: s

  s = ''
  print *, s(1:2_16**64_16+3_16) ! { dg-error "too large" }
  print *, s(2_16**64_16+3_16:2_16**64_16+4_16) ! { dg-error "too large" }
  print *, len(s(1:2_16**64_16+3_16)) ! { dg-error "too large" }
  print *, len(s(2_16**64_16+3_16:2_16**64_16+4_16)) ! { dg-error "too large" }

  print *, s(2_16**64_16+3_16:1)
  print *, s(2_16**64_16+4_16:2_16**64_16+3_16)
  print *, len(s(2_16**64_16+3_16:1))
  print *, len(s(2_16**64_16+4_16:2_16**64_16+3_16))

end subroutine
