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
      write (unit, '(a,a,a,a,i5)') 'x%c="',dtv%c,'",','x%k=', dtv%k
    else
      write (unit,*) dtv%c, dtv%k
    end if
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
  if (buffer.ne.'&NML x%c="a",x%k=    5  /') call abort
  x = t('x', 0)
  read (buffer, nml)
  if (x%c.ne.'a'.or. x%k.ne.5) call abort
end

