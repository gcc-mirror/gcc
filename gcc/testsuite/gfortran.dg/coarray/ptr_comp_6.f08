!{ dg-do run }
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>

program pr104684
  type :: index_map
    integer, allocatable :: send_index(:)
  end type
  type(index_map) :: imap

  imap%send_index = [5,4,3]
  call sub(imap)
contains
  subroutine sub(this)
    type(index_map), intent(inout), target :: this
    type :: box
      integer, pointer :: array(:)
    end type
    type(box), allocatable :: buffer[:]
    allocate(buffer[*])
    buffer%array => this%send_index
    if (any(buffer%array /= [5,4,3])) stop 1    
  end subroutine
end program

