! { dg-do compile }
! PR27954 Internal compiler error on bad statements
! Derived from test case submitted in PR.
subroutine bad1
  character*20 :: y, x 00  ! { dg-error "Syntax error" }
  data  y /'abcdef'/, x /'jbnhjk'/ pp  ! { dg-error "Syntax error" }
end subroutine bad1

subroutine bad2
  character*20 :: y, x 00  ! { dg-error "Syntax error" }
  data  y /'abcdef'/, x /'jbnhjk'/ pp  ! { dg-error "Syntax error" }
  print *, "basket case."
end subroutine bad2

subroutine bad3
  implicit none
  character*20 :: y, x 00  ! { dg-error "Syntax error" }
  data  y /'abcdef'/, x /'jbnhjk'/ pp  ! { dg-error "Syntax error" }
  print *, "basket case that segfaults without patch."
end subroutine bad3

