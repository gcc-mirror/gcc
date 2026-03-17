! { dg-do compile }
!
! Test the fix for pr97818. Used to ICE in structure_alloc_comps, at
! fortran/trans-array.cc:10211 because the parameterized function results
! had not been set.
!
! Contributed by Adam Jermyn  <adamjermyn@gmail.com>
!
module my_type_module
   
   implicit none
      private
   public :: my_type, &
      assignment(=), &
      operator(+)

   type :: my_type(n)
      integer, len :: n ! Length of array of independent variables
      real :: val
      real :: d1
      real :: d2
      real :: d1Array(n)
      real :: d1_d2
      real :: d1_d1Array(n)
      real :: d2_d1Array(n)
      real :: d1_d2_d1Array(n)
   end type my_type
   
   interface assignment(=)
      module procedure assign_from_self
   end interface assignment(=)
   
   interface operator(+)
      module procedure add_self
      module procedure add_self_real
   end interface operator(+)
   
   
   contains

   subroutine assign_from_self(this, other)
      type(my_type(n=*)), intent(inout) :: this
      type(my_type(n=this%n)), intent(in) :: other
   end subroutine assign_from_self

   function add_self(x, y) result(binary)
      type(my_type(n=*)), intent(in) :: x
      type(my_type(n=*)), intent(in) :: y
      type(my_type(n=x%n)) :: binary
   end function add_self
   
   function add_self_real(x, y) result(unary)
      type(my_type(n=*)), intent(in) :: x
      real, intent(in) :: y
      type(my_type(n=x%n)) :: unary
   end function add_self_real

end module my_type_module


program test_auto_diff
   use my_type_module

   call do_test_auto_diff_2var_order3_dArray(5)

   contains

   subroutine do_test_auto_diff_2var_order3_dArray(n)
      integer, intent(in) :: n
      type(my_type(n=n)) :: x, y, z

      z = (x + 2.0) + ((x + 2.0) + 1.0)

   end subroutine do_test_auto_diff_2var_order3_dArray



end program test_auto_diff
