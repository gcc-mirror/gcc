! { dg-do run }
! PR84389 rejected valid use of ':' in format
module m
   type :: t
   integer :: i
   contains
      procedure, pass(this) :: write_t
      generic, public :: write(formatted) => write_t
   end type
contains
   subroutine write_t(this, lun, iotype, vlist, istat, imsg)
      ! argument definitions
      class(t), intent(in)            :: this
      integer, intent(in)             :: lun
      character(len=*), intent(in)    :: iotype
      integer, intent(in)             :: vlist(:)
      integer, intent(out)            :: istat
      character(len=*), intent(inout) :: imsg
      write(lun, fmt=*, iostat=istat, iomsg=imsg) "Hello World!"
   end subroutine write_t
end module
program p
   use m, only : t
   character(50) :: str
   type(t) :: foo(2)
   write(str, "(*(dt:,','))") foo
   if (str.ne." Hello World!, Hello World!") stop 1
end program
