! { dg-do compile }
!
! Test fix for the original in PR793822 and for PR80156.
!
! Contributed by Walt Brainerd  <walt.brainerd@gmail.com>
! and (PR80156)  <pedsxing@gmx.net>
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

   PRIVATE :: write (formatted) ! This used to ICE

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

module pr80156

   implicit none
   private

   type, public :: String
      character(len=:), allocatable :: raw
   end type

   public :: write(unformatted) ! Gave an error due to the first fix for PR79382.
   interface write(unformatted)
      module procedure writeUnformatted
   end interface

contains

   subroutine writeUnformatted(self, unit, iostat, iomsg)
      class(String)   , intent(in)    :: self
      integer         , intent(in)    :: unit
      integer         , intent(out)   :: iostat
      character(len=*), intent(inout) :: iomsg

      if (allocated(self%raw)) then
         write (unit, iostat=iostat, iomsg=iomsg) self%raw
      else
         write (unit, iostat=iostat, iomsg=iomsg) ''
      endif

   end subroutine

end module

  use dollar_mod
  type(dollar_type) :: money
  money = dollar_type(50.0)
  print '(DT)', money ! Make sure that the typebound generic is accessible.
end
