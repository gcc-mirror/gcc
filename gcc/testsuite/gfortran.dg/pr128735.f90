! { dg-do run }
!
! PR 128735
! END ASSOCIATE and END SELECT must not restore the descriptor that a
! polymorphic array selector had on entry to the block.

module m
  type t
    integer :: i
  end type
  type holder
    class(*), pointer :: p(:) => null()
  end type
end module

program p
  use m
  implicit none
  class(*), pointer :: q(:)
  class(t), pointer :: r(:)
  type(holder) :: obj
  integer, target :: a(2) = [1, 2], b(3) = [3, 4, 5]
  character(3), target :: c1(2) = ['abc', 'def']
  character(5), target :: c2(3) = ['ghijk', 'lmnop', 'qrstu']
  type(t), target :: ta(2) = t(1), tb(3) = t(2)

  q => a
  associate (x => q)
    q => b
  end associate
  if (size (q) /= 3) stop 1

  obj%p => a
  select type (z => obj%p)
    type is (integer)
      obj%p => b
      if (any (z /= [1, 2])) stop 2
  end select
  if (size (obj%p) /= 3) stop 3

  q => c1
  associate (x => q)
    q => c2
  end associate
  select type (q)
    type is (character(*))
      if (len (q) /= 5 .or. size (q) /= 3) stop 4
  end select

  r => ta
  associate (x => r)
    x(1)%i = 99
    r => tb
  end associate
  if (size (r) /= 3 .or. ta(1)%i /= 99) stop 5

  call realloc
  call dealloc
contains
  subroutine realloc
    class(t), allocatable :: s(:)
    allocate (s(2), source=t(1))
    associate (x => s)
      deallocate (s)
      allocate (s(4), source=t(2))
    end associate
    if (size (s) /= 4 .or. any (s%i /= 2)) stop 6
  end subroutine
  subroutine dealloc
    class(*), allocatable :: s(:)
    allocate (s, source=[1, 2])
    associate (x => s)
      deallocate (s)
    end associate
    if (allocated (s)) stop 7
  end subroutine
end program
