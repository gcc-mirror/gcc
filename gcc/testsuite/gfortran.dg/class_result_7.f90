! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
!  Test the fix for PR80477
!
! Contributed by Stefano Zaghi  <stefano.zaghi@cnr.it>
!
module a_type_m
   implicit none
   type :: a_type_t
      real :: x
   endtype
contains
   subroutine assign_a_type(lhs, rhs)
      type(a_type_t), intent(inout) :: lhs
      type(a_type_t), intent(in)    :: rhs
      lhs%x = rhs%x
   end subroutine

   function add_a_type(lhs, rhs) result( res )
      type(a_type_t), intent(in)  :: lhs
      type(a_type_t), intent(in)  :: rhs
      class(a_type_t), allocatable :: res
      allocate (a_type_t :: res)
      res%x = lhs%x + rhs%x
   end function
end module

program polymorphic_operators_memory_leaks
   use a_type_m
   implicit none
   type(a_type_t) :: a = a_type_t(1) , b = a_type_t(2)
   call assign_a_type (a, add_a_type(a,b))              ! generated a memory leak
end
! { dg-final { scan-tree-dump-times "builtin_free" 1 "original" } }
! { dg-final { scan-tree-dump-times "builtin_malloc" 1 "original" } }
