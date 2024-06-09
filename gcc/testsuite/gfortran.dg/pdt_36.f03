! { dg-do run }
!
! Tests the fixes for PR82943.
!
! This test focuses on calling the type bound procedures in a program.
!
! Contributed by Alexander Westbrooks  <ctechnodev@gmail.com>
!
module testmod

   public :: foo

   type, public :: tough_lvl_0(a, b)
       integer, kind :: a = 1
       integer, len :: b
   contains
       procedure :: foo
   end type

   type, public, EXTENDS(tough_lvl_0) :: tough_lvl_1 (c)
       integer, len :: c
   contains
       procedure :: bar
   end type

   type, public, EXTENDS(tough_lvl_1) :: tough_lvl_2 (d)
       integer, len :: d
   contains
       procedure :: foobar
   end type

contains
   subroutine foo(this)
       class(tough_lvl_0(1,*)), intent(inout) :: this
   end subroutine

   subroutine bar(this)
       class(tough_lvl_1(1,*,*)), intent(inout) :: this
   end subroutine

   subroutine foobar(this)
       class(tough_lvl_2(1,*,*,*)), intent(inout) :: this
   end subroutine

end module

PROGRAM testprogram
   USE testmod
   
   TYPE(tough_lvl_0(1,5))     :: test_pdt_0
   TYPE(tough_lvl_1(1,5,6))   :: test_pdt_1
   TYPE(tough_lvl_2(1,5,6,7)) :: test_pdt_2

   CALL test_pdt_0%foo()

   CALL test_pdt_1%foo()
   CALL test_pdt_1%bar()

   CALL test_pdt_2%foo()
   CALL test_pdt_2%bar()
   CALL test_pdt_2%foobar()


END PROGRAM testprogram
 