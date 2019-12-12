! { dg-do run }
! PR84387 Defined output does not work for a derived type that
! has no components 
module m
   type :: t
      private
      !integer :: m_i = 0  !<-- ***
   contains
      private
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
      return
   end subroutine write_t

end module

program p
   use m, only : t
   type(t) :: foo
   print "(dt)", foo ! { dg-output " Hello World!" }
end program
