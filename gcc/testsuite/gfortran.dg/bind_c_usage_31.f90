! { dg-do compile }
! PR fortran/84073 - this was accepted before.
module mod
  use iso_c_binding
  type, bind(c) :: a
     character(len=2,kind=c_char) :: b ! { dg-error "must have length one" }
  end type a
  character(len=2), bind(C) :: c ! { dg-error "must have length one" }
end module mod
