! { dg-do compile }
! { dg-options "-Wall" }
! PR 94270 - this used to give a bogus warning.
! Test case by Ignacio Fernández Galván.
subroutine foo()
external bar
call meh(bar)
call foo_internal()
contains
  subroutine foo_internal()
    call meh(bar)
  end subroutine foo_internal
end subroutine foo
