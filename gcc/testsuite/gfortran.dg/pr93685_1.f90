! { dg-do run }
! PR93685 - ICE in gfc_constructor_append_expr, at fortran/constructor.c:135

program p
  implicit none
  type t
     character, pointer :: a
  end type t
  type u
     integer,   pointer :: i
  end type u
  type(t) :: x
  type(u) :: y
  character, target :: c = 'c'
  integer  , target :: i = 10
  data x%a /c/
  data y%i /i/
  if (x% a /= "c") stop 1
  if (y% i /= 10)  stop 2
end
