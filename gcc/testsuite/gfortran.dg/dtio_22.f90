! { dg-do run }
!
! PR 78848: [OOP] ICE on writing CLASS variable with non-typebound DTIO procedure
!
! Contributed by Mikael Morin <morin-mikael@orange.fr>

module m
  type :: t
    integer :: i = 123
  end type
  interface write(formatted)
    procedure wf
  end interface
contains
  subroutine wf(this, unit, b, c, iostat, iomsg)
    class(t), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: b
    integer, intent(in) :: c(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write (unit, "(i3)", IOSTAT=iostat, IOMSG=iomsg) this%i
  end subroutine
end

program p
  use m
  character(3) :: buffer
  class(t), allocatable :: z
  allocate(z)
  write(buffer,"(DT)") z
  if (buffer /= "123") STOP 1
end
