! { dg-do run }
!
! PR78737: [OOP] linking error with deferred, undefined user-defined derived-type I/O
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>

module object_interface
  character(30) :: buffer(2)
  type, abstract :: object
  contains
    procedure(write_formatted_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type
  abstract interface
    subroutine write_formatted_interface(this,unit,iotype,vlist,iostat,iomsg)
      import object
      class(object), intent(in) :: this
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine
  end interface
  type, extends(object) :: non_abstract_child1
    integer :: i
  contains
    procedure :: write_formatted => write_formatted1
  end type
  type, extends(object) :: non_abstract_child2
    real :: r
  contains
    procedure :: write_formatted => write_formatted2
  end type
contains
  subroutine write_formatted1(this,unit,iotype,vlist,iostat,iomsg)
    class(non_abstract_child1), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,'(a,i2/)') "write_formatted1 => ", this%i
  end subroutine
  subroutine write_formatted2(this,unit,iotype,vlist,iostat,iomsg)
    class(non_abstract_child2), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,'(a,f4.1/)') "write_formatted2 => ", this%r
  end subroutine
  subroutine assert(a)
    class(object):: a
    write(buffer,'(DT)') a
  end subroutine
end module

program p
  use object_interface

  call assert (non_abstract_child1 (99))
  if (trim (buffer(1)) .ne. "write_formatted1 => 99") call abort

  call assert (non_abstract_child2 (42.0))
  if (trim (buffer(1)) .ne. "write_formatted2 => 42.0") call abort
end
