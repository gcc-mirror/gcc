! { dg-do compile }
!
! Tests the fixes for PR82943.
!
! This test focuses on inheritance for the type bound procedures.
!
! Contributed by Alexander Westbrooks  <ctechnodev@gmail.com>
!
module m

   public :: foo, bar, foobar

   type, public :: goodpdt_lvl_0(a, b)
       integer, kind :: a = 1
       integer, len :: b
   contains
       procedure :: foo
   end type

   type, public, EXTENDS(goodpdt_lvl_0) :: goodpdt_lvl_1 (c)
       integer, len :: c
   contains
       procedure :: bar
   end type

   type, public, EXTENDS(goodpdt_lvl_1) :: goodpdt_lvl_2 (d)
       integer, len :: d
   contains
       procedure :: foobar
   end type

contains
   subroutine foo(this)
       class(goodpdt_lvl_0(1,*)), intent(inout) :: this
   end subroutine

   subroutine bar(this)
       class(goodpdt_lvl_1(1,*,*)), intent(inout) :: this
   end subroutine

   subroutine foobar(this)
       class(goodpdt_lvl_2(1,*,*,*)), intent(inout) :: this
   end subroutine

end module