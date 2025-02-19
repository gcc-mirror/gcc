! { dg-do compile }

! PR 24878 - passing a global subroutine as a function, or vice versa,
! was not caught, nor were type mismatches.  Original test case by
! Uttam Pawar.

program memain
  implicit none
  integer subr
  external subr
  external i4
  external r4
  integer r4
  
  call foo(subr) ! { dg-error "Passing global subroutine" }
  call bar(i4)   ! { dg-error "Passing global function" }
  call baz(r4)   ! { dg-error "Type mismatch" }
end program memain

subroutine foo(ifun)
  integer(kind=4) ifun
  external ifun
  integer y
!---FNC is not a Function subprogram so calling it
!   as a function is an error.
  Y=ifun(32)
end subroutine foo

subroutine bar(sub)
  call sub
end subroutine bar

subroutine subr(X) ! { dg-error "Passing global subroutine" }
  integer x
  x = 12345
end subroutine subr

integer(kind=4) function i4() ! { dg-error "Passing global function" }
  i4 = 42
end function i4

real(kind=4) function r4() ! { dg-error "Type mismatch" }
  r4 = 1.0
end function r4
  
subroutine baz(ifun)
  integer(kind=4) ifun
  external ifun
  integer y
  y = ifun(32)
end subroutine baz
