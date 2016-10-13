! { dg-do run }
! { dg-options "-fcheck=bounds" }
! { dg-shouldfail "Array bounds mismatch" }
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
  call test_class_correct()
  call test_class_fail()

contains

subroutine test_class_correct()
  class(t), allocatable, dimension(:) :: a, b
  allocate(tt::a(1:2))
  a(:)%i = [ 1,2 ]
  if (size(a) /= 2) call abort()
  if (any(a(:)%i /= [ 1,2])) call abort()

  allocate(b(1:4), source=a(1))
  if (size(b) /= 4) call abort()
  if (any(b(:)%i /= [ 1,1,1,1])) call abort()
  select type (b(1))
    class is (tt)
      continue
    class default
      call abort()
  end select
end subroutine

subroutine test_class_fail()
  class(t), allocatable, dimension(:) :: a, b
  allocate(tt::a(1:2))
  a(:)%i = [ 1,2 ]
  if (size(a) /= 2) call abort()
  if (any(a(:)%i /= [ 1,2])) call abort()

  allocate(b(1:4), source=a) ! Fail expected: sizes do not conform
  if (size(b) /= 4) call abort()
  if (any(b(1:2)%i /= [ 1,2])) call abort()
  select type (b(1))
    class is (tt)
      continue
    class default
      call abort()
  end select
end subroutine

subroutine test_type()
  type(t), allocatable, dimension(:) :: a, b
  allocate(a(1:2))
  if (size(a) /= 2) call abort()

  allocate(b(1:4), source=a)
  if (size(b) /= 4) call abort()
end subroutine
end program allocate_source


