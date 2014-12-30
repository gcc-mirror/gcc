! { dg-do run }
! PR46990 - class array implementation
!
! Contributed by Wolfgang Kilian on comp.lang.fortran - see comment #7 of PR
!
module realloc
  implicit none

  type :: base_type
     integer :: i
  contains
    procedure :: assign
    generic :: assignment(=) => assign   ! define generic assignment
  end type base_type

  type, extends(base_type) :: extended_type
     integer :: j
  end type extended_type

contains

  impure elemental subroutine assign (a, b)
    class(base_type), intent(out) :: a
    type(base_type), intent(in) :: b
    a%i = b%i
  end subroutine assign

  subroutine reallocate (a)
    class(base_type), dimension(:), allocatable, intent(inout) :: a
    class(base_type), dimension(:), allocatable :: tmp
    allocate (tmp (2 * size (a))) ! how to alloc b with same type as a ?
    if (trim (print_type ("tmp", tmp)) .ne. "tmp is base_type") call abort
    tmp(:size(a)) = a             ! polymorphic l.h.s.
    call move_alloc (from=tmp, to=a)
  end subroutine reallocate

  character(20) function print_type (name, a)
    character(*), intent(in) :: name
    class(base_type), dimension(:), intent(in) :: a
    select type (a)
     type is (base_type);      print_type = NAME // " is base_type"
     type is (extended_type);  print_type = NAME // " is extended_type"
    end select
  end function

end module realloc

program main
  use realloc
  implicit none
  class(base_type), dimension(:), allocatable :: a

  allocate (extended_type :: a(10))
  if (trim (print_type ("a", a)) .ne. "a is extended_type") call abort
  call reallocate (a)
  if (trim (print_type ("a", a)) .ne. "a is base_type") call abort
  deallocate (a)
end program main
