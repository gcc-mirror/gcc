! { dg-do compile }
!

program test
  integer, parameter :: n1 = 1
  implicit type(t) (n1)  ! { dg-error "Syntax error" }
  type t
  end type
end program


