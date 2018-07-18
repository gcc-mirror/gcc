! { dg-do compile }
! PR80484 Three syntax errors involving derived-type I/O
module dt_write_mod
   type, public :: B_type
      real :: amount
   end type B_type
   interface write (formatted)
      procedure :: Write_b
   end interface
contains

subroutine Write_b &
   (amount, unit, b_edit_descriptor, v_list, iostat, iomsg)

   class (B_type), intent(in) :: amount
   integer, intent(in) :: unit
   character (len=*), intent(in) :: b_edit_descriptor
   integer, dimension(:), intent(in) :: v_list
   integer, intent(out) :: iostat
   character (len=*), intent(inout) :: iomsg
   write (unit=unit, fmt="(f9.3)", iostat=iostat) amount%amount

end subroutine Write_b

end module dt_write_mod

program test
   use dt_write_mod, only: B_type  , write(formatted)
   implicit none

   real :: wage = 15.10
   integer :: ios
   character(len=99) :: iom = "OK"

   write (unit=*, fmt="(DT'$$$Z.##')", iostat=ios, iomsg=iom) &
     B_type(wage), B_type(wage)
   print *, trim(iom)
   write (unit=*, fmt="(2DT'$$$Z.##')", iostat=ios, iomsg=iom) &
     B_type(wage), B_type(wage)
   print *, trim(iom)
   write (unit=*, fmt="(3DT'$$$Z.##')", iostat=ios, iomsg=iom) &
     B_type(wage), B_type(wage)
   print *, trim(iom)
   write (unit=*, fmt="(DT'$$$Z.##'/)", iostat=ios, iomsg=iom) &
     B_type(wage), B_type(wage)
   print *, trim(iom)
end program test
