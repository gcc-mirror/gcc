! { dg-do run }
!
! Test the fix for PR77657 in which the DTIO subroutine was not found,
! which led to an error in attempting to link to the abstract interface.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
MODULE abstract_parent
  implicit none

  type, abstract :: parent
  contains
    procedure(write_formatted_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type parent

  abstract interface
    subroutine write_formatted_interface(this,unit,iotype,vlist,iostat,iomsg)
      import parent
      class(parent), intent(in) :: this
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine
  end interface

end module

module child_module
  use abstract_parent, only : parent
  implicit none

  type, extends(parent) :: child
    integer :: i = 99
  contains
    procedure :: write_formatted
  end type
contains
  subroutine write_formatted(this,unit,iotype,vlist,iostat,iomsg)
    class(child), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write (unit, "(i4)") this%i
  end subroutine
end module

  use child_module, only : child
  implicit none
  type (child) :: baby
  integer :: v(1), istat
  character(20) :: msg
  open (10, status = "scratch")
  call baby%write_formatted(10, "abcd", v, istat, msg) ! Call the dtio proc directly
  rewind (10)
  read (10, *) msg
  if (trim (msg) .ne. "99") call abort
  rewind (10)
  baby%i = 42
  write (10,"(DT)") baby                               ! Call the dtio proc via the library
  rewind (10)
  read (10, *) msg
  if (trim (msg) .ne. "42") call abort
  rewind (10)
  write (10,"(DT)") child (77)                         ! The original testcase
  rewind (10)
  read (10, *) msg
  if (trim (msg) .ne. "77") call abort
  close(10)
end
