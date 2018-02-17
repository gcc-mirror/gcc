! { dg-do run }
!
! Test that pr72832 is fixed now.
! Contributed by Daan van Vugt

program allocate_source
  type :: t
    integer :: i
  end type t
  type, extends(t) :: tt
  end type tt

  call test_type()
  call test_class()

contains

subroutine test_class()
  class(t), allocatable, dimension(:) :: a, b
  allocate(tt::a(1:2))
  a(:)%i = [ 1,2 ]
  if (size(a) /= 2) STOP 1
  if (any(a(:)%i /= [ 1,2])) STOP 2

  allocate(b(1:4), source=a)
  ! b is incorrectly initialized here.  This only is diagnosed when compiled
  ! with -fcheck=bounds.
  if (size(b) /= 4) STOP 3
  if (any(b(1:2)%i /= [ 1,2])) STOP 4
  select type (b1 => b(1))
    class is (tt)
      continue
    class default
      STOP 5
  end select
end subroutine

subroutine test_type()
  type(t), allocatable, dimension(:) :: a, b
  allocate(a(1:2))
  if (size(a) /= 2) STOP 6

  allocate(b(1:4), source=a)
  if (size(b) /= 4) STOP 7
end subroutine
end program allocate_source


