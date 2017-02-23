! { dg-do compile }
!
! Test fix for the original in PR79832.
!
! Contributed by Walt Brainerd  <walt.brainerd@gmail.com>
!
module dollar_mod

   implicit none
   private

   type, public :: dollar_type
      real :: amount
   contains
      procedure :: Write_dollar
      generic :: write(formatted) => Write_dollar
   end type dollar_type

   PRIVATE :: write (formatted) ! { dg-error "is not present" }

contains

subroutine Write_dollar &

   (dollar_value, unit, b_edit_descriptor, v_list, iostat, iomsg)

   class (dollar_type), intent(in) :: dollar_value
   integer, intent(in) :: unit
   character (len=*), intent(in) :: b_edit_descriptor
   integer, dimension(:), intent(in) :: v_list
   integer, intent(out) :: iostat
   character (len=*), intent(inout) :: iomsg
   write (unit=unit, fmt="(f9.2)", iostat=iostat) dollar_value%amount

end subroutine Write_dollar

end module dollar_mod
