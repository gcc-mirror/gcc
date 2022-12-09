! { dg-do run }
!
! Test the fix for PR107872, where an ICE occurred in
! resolve.cc(derived_inaccessible) because derived types with
! recursive allocatable components were not catered for.
!
module mod1
  type t
     integer :: data
     type(t), allocatable :: next
   contains
     procedure, private :: write_t
     generic :: write(formatted) => write_t
  end type
contains
  recursive subroutine write_t(this, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    if (ALLOCATED(this%next)) &
         write (unit, '(dt)') this%next
    write (unit, '(i2)') this%data
  end subroutine
end module

  use mod1
  type(t) :: a
  character (8) :: buffer
  a%data = 1
  allocate (a%next)
  a%next%data = 2
  allocate (a%next%next)
  a%next%next%data = 3
  write (buffer, '(dt)')a
  deallocate (a%next)
  if (trim (buffer) .ne. ' 3 2 1') stop 1
end
