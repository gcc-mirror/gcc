! { dg-do run }
! { dg-options "-w" }
! PR fortran/79383
! Contributed by Walt Brainerd <walt.brainerd at gmail dot com>
module dollar_mod

   implicit none

   private

   type, public :: dollar_type
      real :: amount
   end type dollar_type

   interface write(formatted)
      procedure :: Write_dollar
   end interface

   public :: write(formatted)

   contains

      subroutine Write_dollar(dollar_value, unit, b_edit_descriptor, &
      &  v_list, iostat, iomsg)
         class(dollar_type), intent(in) :: dollar_value
         integer, intent(in) :: unit
         character(len=*), intent(in) :: b_edit_descriptor
         integer, dimension(:), intent(in) :: v_list
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg
         write(unit=unit, fmt="(f9.2)", iostat=iostat) dollar_value%amount
      end subroutine Write_dollar

end module dollar_mod

program test_dollar

   use :: dollar_mod  ! with this USE, same result
   implicit none

   type(dollar_type), parameter :: wage = dollar_type(15.10)
   character(len=10) str
   write(str, fmt="(DT)") wage
   if (trim(adjustl(str)) /= '15.10') STOP 1

end program test_dollar
