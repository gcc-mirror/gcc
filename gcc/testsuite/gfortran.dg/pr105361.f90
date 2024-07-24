! { dg-do run }

module x
  implicit none
  type foo
     real :: r
  end type foo
  interface read(formatted)
     module procedure read_formatted
  end interface read(formatted)
contains
  subroutine read_formatted (dtv, unit, iotype, vlist, iostat, iomsg)
    class (foo), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    read (unit,*,iostat=iostat,iomsg=iomsg) dtv%r
    !print *,dtv%r
  end subroutine read_formatted
end module x

program main
  use x
  implicit none
  type(foo) :: a, b
  real :: c, d
  open(10, access="stream") 
  write(10) "1 2" ! // NEW_LINE('A')
  close(10)
  open(10)
  read(10,*) c, d
  if ((c /= 1.0) .or. (d /= 2.0)) stop 1
  rewind(10)
  !print *, c,d
  read (10,*) a, b
  close(10, status="delete")
  if ((a%r /= 1.0) .or. (b%r /= 2.0)) stop 2
  !print *, a,b
end program main
