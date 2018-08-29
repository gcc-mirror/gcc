! { dg-do run }
!
! Test fix for the additional bug that was found in fixing PR79832.
!
! Contributed by Walt Brainerd  <walt.brainerd@gmail.com>
!
module dollar_mod

   implicit none
   private

   type, public :: dollar_type
      real :: amount
   end type dollar_type

   interface write(formatted)
      module procedure Write_dollar
   end interface

   private :: write (formatted)

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

program test_dollar

   use :: dollar_mod
   implicit none
   integer  :: ios
   character(100) :: errormsg

   type (dollar_type), parameter :: wage = dollar_type(15.10)
   write (unit=*, fmt="(DT)", iostat=ios, iomsg=errormsg) wage
   if (ios.ne.5006) STOP 1
   if (errormsg(1:22).ne."Missing DTIO procedure") STOP 2
end program test_dollar
