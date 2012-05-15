! { dg-do compile }
! PR fortran/41369 - rejected empty type in function return values

module m
 type t
 end type t
end module

type(t) function foo()
  use m
  foo = t()
end function foo
