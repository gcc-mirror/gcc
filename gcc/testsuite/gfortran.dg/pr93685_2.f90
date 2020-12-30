! { dg-do compile }
! PR93685 - ICE in gfc_constructor_append_expr, at fortran/constructor.c:135

program p
  implicit none
  type t
     character :: a
  end type t
  type u
     integer   :: i
  end type u
  type(t) :: x
  type(u) :: y
  character, target :: c = 'c'
  integer  , target :: i = 10
  data x%a /c/  ! { dg-error "non-constant initialization expression" }
  data y%i /i/  ! { dg-error "non-constant initialization expression" }
end
