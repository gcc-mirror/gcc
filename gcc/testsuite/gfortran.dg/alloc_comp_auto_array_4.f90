!{ dg-do run }

! Check freeing derived typed result's allocatable components is not done twice.
! Contributed by Damian Rouson  <damian@archaeologic.codes>

program pr118747
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  end type

  call check_allocation([foo(), foo()])

contains

  type(string_t) function foo()
    foo%string_ = "foo"
  end function

  elemental subroutine check_allocation(string)
    type(string_t), intent(in) ::  string
    if (.not. allocated(string%string_)) error stop "unallocated"
  end subroutine

end program

