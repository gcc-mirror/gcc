! { dg-do run }
!
!  Test the fix for an additional bug found while fixing PR80477
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
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
      lhs%y = rhs(1)%y + rhs(2)%y
   end subroutine

   function add_a_type(lhs, rhs) result( res )
      type(a_type_t), intent(in)  :: lhs
      type(a_type_t), intent(in)  :: rhs
      class(a_type_t), allocatable :: res(:)
      allocate (a_type_t :: res(2))
      allocate (res(1)%y(1), source = [10.0])
      allocate (res(2)%y(1), source = [20.0])
      res(1)%x = lhs%x + rhs%x
      res(2)%x = rhs%x + rhs%x
   end function
end module

program polymorphic_operators_memory_leaks
    use a_type_m
    implicit none
    type(a_type_t) :: a = a_type_t(1) , b = a_type_t(2)
    class(a_type_t), allocatable :: res(:)

    res = add_a_type(a,b)        ! Remarkably, this ICEd - found while debugging the PR.
    call assign_a_type (a, res)
    if (int (res(1)%x + res(2)%x) .ne. int (a%x)) stop 1
    if (int (sum (res(1)%y + res(2)%y)) .ne. int (sum (a%y))) stop 1
    deallocate (a%y)
    deallocate (res)
end
