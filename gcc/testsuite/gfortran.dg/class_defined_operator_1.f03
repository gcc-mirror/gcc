! { dg-do run }
! Test the fix for PR42385, in which CLASS defined operators
! compiled but were not correctly dynamically dispatched.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
module foo_module
 implicit none
 private
 public :: foo

 type :: foo
   integer :: foo_x
 contains
   procedure :: times => times_foo
   procedure :: assign => assign_foo
   generic :: operator(*) => times
   generic :: assignment(=) => assign
 end type

contains

   function times_foo(this,factor) result(product)
     class(foo) ,intent(in) :: this
     class(foo) ,allocatable :: product
     integer, intent(in) :: factor
     allocate (product, source = this)
     product%foo_x = -product%foo_x * factor
   end function

   subroutine assign_foo(lhs,rhs)
     class(foo) ,intent(inout) :: lhs
     class(foo) ,intent(in) :: rhs
     lhs%foo_x = -rhs%foo_x
   end subroutine

end module

module bar_module
 use foo_module ,only : foo
 implicit none
 private
 public :: bar

 type ,extends(foo) :: bar
   integer :: bar_x
 contains
   procedure :: times => times_bar
   procedure :: assign => assign_bar
 end type

contains
 subroutine assign_bar(lhs,rhs)
   class(bar) ,intent(inout) :: lhs
   class(foo) ,intent(in) :: rhs
   select type(rhs)
     type is (bar)
       lhs%bar_x = rhs%bar_x
       lhs%foo_x = -rhs%foo_x
   end select
 end subroutine
 function times_bar(this,factor) result(product)
   class(bar) ,intent(in) :: this
   integer, intent(in) :: factor
   class(foo), allocatable :: product
   select type(this)
     type is (bar)
       allocate(product,source=this)
       select type(product)
         type is(bar)
           product%bar_x = 2*this%bar_x*factor
       end select
   end select
 end function
end module

program main
 use foo_module ,only : foo
 use bar_module ,only : bar
 implicit none
 type(foo) :: unitf
 type(bar) :: unitb

! foo's assign negates, whilst its '*' negates and mutliplies.
 unitf%foo_x = 1
 call rescale(unitf, 42)
 if (unitf%foo_x .ne. 42) STOP 1

! bar's assign negates foo_x, whilst its '*' copies foo_x
! and does a multiply by twice factor.
 unitb%foo_x = 1
 unitb%bar_x = 2
 call rescale(unitb, 3)
 if (unitb%bar_x .ne. 12) STOP 2
 if (unitb%foo_x .ne. -1) STOP 3
contains
 subroutine rescale(this,scale)
   class(foo) ,intent(inout) :: this
   integer, intent(in) :: scale
   this = this*scale
 end subroutine
end program
