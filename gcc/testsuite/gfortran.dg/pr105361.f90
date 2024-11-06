! { dg-do run }

module x
  implicit none
  type foo
     real(4) :: r
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
  real(4) :: c, d
  c = 0.0_4
  d = 0.0_4
  open(10, access="stream") 
  write(10) "1 2" // NEW_LINE('A')
  close(10)
  open(10)
  read(10,*) c, d
  if ((abs(c - 1.0_4) .gt. 0.001_4) .or. (abs(d - 2.0_4) .gt. 0.001_4)) stop 1
  rewind(10)
  !print *, c,d
  a%r = 0.0_4
  b%r = 0.0_4
  read (10,*) a, b
  close(10, status="delete")
  if ((abs(a%r - 1.0_4) .gt. 0.001_4) .or. (abs(b%r - 2.0_4) .gt. 0.001_4)) stop 2
  !print *, a,b
end program main
