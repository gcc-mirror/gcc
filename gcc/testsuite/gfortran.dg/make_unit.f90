! { dg-do run }
! PR61933, useing inquire to get available units.
program makeunit
integer  :: ic, istat, nc
logical  :: exists, is_open

if (get_unit_number("foo0.dat") .ne. 10) call abort
if (get_unit_number("foo1.dat") .ne. 11) call abort
if (get_unit_number("foo2.dat") .ne. 12) call abort
if (get_unit_number("foo3.dat") .ne. 13) call abort

close(unit=12, status="delete")
if (get_unit_number("foo2.dat") .ne. 12) call abort()
close(unit=10, status="delete")
close(unit=11, status="delete")
close(unit=12, status="delete")
close(unit=13, status="delete")

contains
  function get_unit_number(file_name) result(unit_number)
    character(len=*), intent(in), optional   :: file_name
    integer                                  :: unit_number
    ! get a new unit number
    do unit_number=10,100
       inquire (unit=unit_number,exist=exists,opened=is_open,iostat=istat)
       if (exists.and.(.not.is_open).and.(istat == 0)) then
           open(unit=unit_number, file=file_name)
           return
       endif
    end do
    unit_number = -1
  end function get_unit_number

end program makeunit
