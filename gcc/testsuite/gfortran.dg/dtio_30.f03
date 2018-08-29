! { dg-do run }
! PR80333  Namelist dtio write of array of class does not traverse the array
! This test checks both NAMELIST WRITE and READ of an array of class
module m
  implicit none
  type :: t
    character :: c
    character :: d
  contains
    procedure :: read_formatted
    generic :: read(formatted) => read_formatted
    procedure :: write_formatted
    generic :: write(formatted) => write_formatted
  end type t
contains
  subroutine read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i
    read(unit,'(a1,a1)', iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
  end subroutine read_formatted

  subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit,'(a1,a1)', iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
  end subroutine write_formatted
end module m

program p
  use m
  implicit none
  class(t), dimension(:,:), allocatable :: w
  namelist /nml/  w
  integer :: unit, iostatus
  character(256) :: str = ""

  open(10, status='scratch')
  allocate(w(10,3))
  w = t('j','r')
  w(5:7,2)%c='k'
  write(10, nml)
  rewind(10)
  w = t('p','z')
  read(10, nml)
  write(str,*) w
  if (str.ne." jr jr jr jr jr jr jr jr jr jr jr jr jr jr kr kr kr jr jr jr jr jr jr jr jr jr jr jr jr jr") &
      & STOP 1
  str = ""
  write(str,"(*(DT))") w
  if (str.ne."jrjrjrjrjrjrjrjrjrjrjrjrjrjrkrkrkrjrjrjrjrjrjrjrjrjrjrjrjrjr") STOP 2
end program p
