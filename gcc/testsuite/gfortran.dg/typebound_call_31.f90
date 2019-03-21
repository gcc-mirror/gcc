! { dg-do compile }
! PR 88008 - this use to ICE. Original test case by
! Gerhard Steinmetz.

module m
   type t
      integer, pointer :: z
   contains
      procedure :: g
   end type
contains
   subroutine g(x)
      class(t) :: x
      call x%z%g()  ! { dg-error "Error in typebound call" }
   end
end
