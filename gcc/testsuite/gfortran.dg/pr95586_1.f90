! { dg-do compile }
!

program test
  implicit type(t) (1)  ! { dg-error "Syntax error" }
  type t
  end type
end program

