! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
!  Test the fix for the array version of PR80477
!
! Contributed by Stefano Zaghi  <stefano.zaghi@cnr.it>
!
module a_type_m
   implicit none
   type :: a_type_t
      real :: x
      real, allocatable :: y(:)
   endtype
contains
   subroutine assign_a_type(lhs, rhs)
      type(a_type_t), intent(inout) :: lhs
      type(a_type_t), intent(in)    :: rhs(:)
      lhs%x = rhs(1)%x + rhs(2)%x
   end subroutine

   function add_a_type(lhs, rhs) result( res )
      type(a_type_t), intent(in)  :: lhs
      type(a_type_t), intent(in)  :: rhs
      class(a_type_t), allocatable :: res(:)
      allocate (a_type_t :: res(2))
      allocate (res(1)%y(1))
      allocate (res(2)%y(1))
      res(1)%x = lhs%x
      res(2)%x = rhs%x
   end function
end module

program polymorphic_operators_memory_leaks
   use a_type_m
   implicit none
   type(a_type_t) :: a = a_type_t(1) , b = a_type_t(2)
   call assign_a_type (a, add_a_type(a,b))
   print *, a%x
end
! { dg-final { scan-tree-dump-times "builtin_free" 5 "original" } }
! { dg-final { scan-tree-dump-times "builtin_malloc" 7 "original" } }
