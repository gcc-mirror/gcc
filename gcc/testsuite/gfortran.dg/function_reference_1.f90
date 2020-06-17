! { dg-do compile }
! PR 44960 - this was erroneusly accepted.
! Original test case by Daniel Franke.

type t
  integer :: a
end type t
type(t) :: foo
print *, foo(1)%a ! { dg-error "Unexpected junk" }
end

