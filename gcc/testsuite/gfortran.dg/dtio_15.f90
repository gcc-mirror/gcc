! { dg-do run }
! Test that inquire of string internal unit in child process errors.
module string_m
  implicit none
  type person
    character(10) :: aname
    integer :: ijklmno
  contains
    procedure :: write_s
    generic :: write(formatted) => write_s
  end type person
contains
  subroutine write_s (this, lun, iotype, vlist, istat, imsg)
    class(person), intent(in)       :: this
    integer, intent(in)             :: lun
    character(len=*), intent(in)    :: iotype
    integer, intent(in)             :: vlist(:)
    integer, intent(out)            :: istat
    character(len=*), intent(inout) :: imsg
    integer :: filesize
    inquire( unit=lun, size=filesize, iostat=istat, iomsg=imsg)
    if (istat /= 0) return
  end subroutine write_s
end module string_m
program p
   use string_m
   type(person) :: s
   character(len=12) :: msg
   integer :: istat
   character(len=256) :: imsg = ""
   write( msg, "(DT)", iostat=istat) s
   if (istat /= 5018) call abort
end program p
