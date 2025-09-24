! { dg-do run }
!
! Check the fix for pr87908, which used to fail with error:
! Procedure ‘__vtab_m_T’ in generic interface '_dtio_formatted_read' at (1) is
! neither function nor subroutine.
!
! Contributed by David Bolvansky  <david.bolvansky@gmail.com>
!
module m
   type t
      character(34) :: c
   contains
      procedure :: g
      generic :: read(formatted) => g
   end type
   integer :: ctr = 0
contains
   subroutine s (unit, x)
      integer, intent(in) :: unit
      integer, intent(in) :: x(:)
      interface read(formatted)
         procedure g
      end interface
   end
   subroutine g (dtv, unit, iotype, v_list, iostat, iomsg)
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
      read (unit, '(a)', iostat=iostat, iomsg=iomsg) dtv%c
      ctr = ctr + 1
   end
end

  use m
  type(t) :: x
  open (10, status = 'scratch')
  write(10, fmt=*) "Mary had a little lamb            "
  write(10, fmt=*) "whose fleece was as white as gold "
  rewind(10)
  read(10, fmt=*) x
  if (trim(x%c) /= "Mary had a little lamb") stop 1
  read(10, fmt=*) x
  if (trim(x%c) /= "whose fleece was as white as gold") stop 2
  close(10)
  if (ctr /= 2) stop 3
end
