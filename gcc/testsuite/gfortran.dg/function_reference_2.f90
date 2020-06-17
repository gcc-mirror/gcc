! { dg-do compile }
! PR 44960 - improve the error message
program main
  type t
  integer :: a
end type t
type(t) :: foo
external foo
i = foo(1)%1 ! { dg-error "leftmost part-ref in a data-ref cannot be a function reference" }
end
