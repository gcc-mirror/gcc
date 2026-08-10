! { dg-do run }
! PR fortran/53800

! Further cases in which a dummy must be associated with the actual
! argument's storage rather than a copy-in/copy-out temporary: an
! intrinsic-type component of a CLASS array, a POINTER dummy and an
! assumed-rank TARGET dummy.
!
! Variations contributed by Mikael Morin  <mikael@gcc.gnu.org>

module m
  implicit none
  type t
    integer :: i
  end type t
  type, extends(t) :: t2
    integer :: j
  end type t2
end module m

! An intrinsic-type component of a CLASS array to an INTEGER TARGET dummy.
subroutine test_integer_component ()
  use m
  implicit none
  class(t), target, allocatable :: a(:,:)
  integer, pointer :: ptr

  allocate (t2 :: a(5,5))
  a(:,:)%i = 53
  a(3,3)%i = 42

  call f (a%i)
  if (ptr /= 42) stop 1
  a(3,3)%i = 999
  if (ptr /= 999) stop 2
contains
  subroutine f(x)
    integer, target :: x(:,:)
    ptr => x(3,3)
  end subroutine f
end subroutine test_integer_component

! A component of a plain derived-type array to an INTEGER TARGET dummy.
subroutine test_subref_component ()
  implicit none
  type u
    integer :: i
    integer :: pad
  end type u
  type(u), target :: a(5,5)
  integer, pointer :: ptr

  a(:,:)%i = 53
  a(3,3)%i = 42

  call f (a%i)
  if (ptr /= 42) stop 3
  a(3,3)%i = 999
  if (ptr /= 999) stop 4
contains
  subroutine f(x)
    integer, target :: x(:,:)
    ptr => x(3,3)
  end subroutine f
end subroutine test_subref_component

! A character component of a derived-type array to a CHARACTER TARGET dummy.
subroutine test_character_component ()
  implicit none
  type u
    character(len=4) :: c
    integer :: pad
  end type u
  type(u), target :: a(6)
  character(len=4), pointer :: ptr
  integer :: k

  do k = 1, 6
    a(k)%c = "ab00"
  end do
  a(4)%c = "zzzz"

  call f (a%c)
  if (ptr /= "zzzz") stop 10
  a(4)%c = "qqqq"
  if (ptr /= "qqqq") stop 11
contains
  subroutine f(x)
    character(len=4), target :: x(:)
    ptr => x(4)
  end subroutine f
end subroutine test_character_component

! A CLASS POINTER array to a TYPE POINTER dummy.
subroutine test_pointer_dummy ()
  use m
  implicit none
  class(t), pointer :: a(:,:)
  type(t), pointer :: ptr

  allocate (t2 :: a(5,5))
  a(:,:)%i = 53
  a(3,3)%i = 42

  call f (a)
  if (ptr%i /= 42) stop 5
  a(3,3)%i = 999
  if (ptr%i /= 999) stop 6
  deallocate (a)
contains
  subroutine f(x)
    type(t), pointer :: x(:,:)
    ptr => x(3,3)
  end subroutine f
end subroutine test_pointer_dummy

! A CLASS array to an assumed-rank TARGET dummy, selected with SELECT RANK.
subroutine test_assumed_rank ()
  use m
  implicit none
  class(t), target, allocatable :: a(:,:)
  type(t), pointer :: ptr

  allocate (t2 :: a(5,5))
  a(:,:)%i = 53
  a(3,3)%i = 42

  call f (a)
  if (ptr%i /= 42) stop 7
  a(3,3)%i = 999
  if (ptr%i /= 999) stop 8
contains
  subroutine f(x)
    type(t), target :: x(..)
    select rank (x)
      rank (2)
        ptr => x(3,3)
      rank default
        error stop 9
    end select
  end subroutine f
end subroutine test_assumed_rank

program class_to_type_7
  implicit none
  call test_integer_component ()
  call test_subref_component ()
  call test_character_component ()
  call test_pointer_dummy ()
  call test_assumed_rank ()
end program class_to_type_7
