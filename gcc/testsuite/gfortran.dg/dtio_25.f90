! { dg-do run }
! PR78854 namelist write to internal unit.
module m
  implicit none
  type :: t
    character :: c
    integer :: k
  contains
    procedure :: write_formatted
    generic :: write(formatted) => write_formatted
    procedure :: read_formatted
    generic :: read(formatted) => read_formatted
  end type
contains
  subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    if (iotype.eq."NAMELIST") then
      write (unit, '(a1,a1,i3)') dtv%c,',', dtv%k
    else
      write (unit,*) dtv%c, dtv%k
    end if
  end subroutine
  subroutine read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character :: comma
    if (iotype.eq."NAMELIST") then
      read (unit, '(a1,a1,i3)') dtv%c, comma, dtv%k
    else
      read (unit,*) dtv%c, comma, dtv%k
    end if
    if (comma /= ',') STOP 1
  end subroutine
end module

program p
  use m
  implicit none
  character(len=50) :: buffer
  type(t) :: x
  namelist /nml/ x
  x = t('a', 5)
  write (buffer, nml)
  if (buffer.ne.' &NML  X=a,  5  /') STOP 1
  x = t('x', 0)
  read (buffer, nml)
  if (x%c.ne.'a'.or. x%k.ne.5) STOP 2
end
