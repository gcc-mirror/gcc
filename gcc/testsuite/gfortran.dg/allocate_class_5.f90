! { dg-do run }
!
! Tests the fix of PR98573. Fixed missing vptrs for class allocations.
!
! Contributed by Davis Asanza  <davidhneill@gmail.com>
!
module counts
  integer :: integer_count = 0
  integer :: other_count = 0
  integer :: alloc_counts = 0
end module counts

module foo1
  use counts
  type, public:: box
    class(*), allocatable :: val(:)
  end type
contains
  subroutine store1(this, val)
    class(box), intent(out) :: this
    class(*), intent(in) :: val(:)
    this%val = val
  end subroutine store1
  subroutine store2(this, val)
    class(box), intent(out) :: this
    class(*), intent(in) :: val(:)
    allocate(this%val, source=val)
  end subroutine store2
  subroutine vector_type(val)
    class(*), intent(in) :: val(:)
    select type (val)
    type is (integer)
      integer_count = integer_count + 1
    class default
      other_count = other_count + 1
    end select
  end subroutine vector_type
end module foo1

module foo2
  use counts
contains
  subroutine store1(arr, val)
    class(*), allocatable, intent(out) :: arr(:)
    class(*), intent(in) :: val(:)
    arr = val
  end subroutine store1
  subroutine store2(arr, val)
    class(*), allocatable, intent(out) :: arr(:)
    class(*), intent(in) :: val(:)
    allocate(arr, source=val)
  end subroutine store2
end module foo2

module foo3
  use counts
  type, public:: box
    class(*), allocatable :: val(:)
  end type
contains
  subroutine store1(this, val)
    class(box), intent(out) :: this
    class(*), intent(in) :: val(:)
    this%val = val
  end subroutine store1
  subroutine store2(this, val)
    class(box), intent(out) :: this
    class(*), intent(in) :: val(:)
    allocate(this%val, source=val)
  end subroutine store2
  subroutine vector_type(val)
    class(*), intent(in) :: val(:)
    select type (val)
    type is (integer)
      integer_count = integer_count + 1
    class default
      other_count = other_count + 1
    end select
  end subroutine vector_type
end module foo3

program prog
  use counts
  implicit none
  call bar1  ! Test the original problem
  call bar2  ! Test comment 1
  call bar3  ! Test comment 3
  if (integer_count .ne. 6) stop 1
  if (other_count .ne. 0) stop 2
  if (alloc_counts .ne. 2) stop 3
contains
  subroutine bar1
    use foo1
    type(box) :: b
    call store1(b, [1, 2, 3])
    call vector_type(b%val)  ! OTHER
    call store2(b, [1, 2, 3])
    call vector_type(b%val)  ! INTEGER
  end subroutine bar1

  subroutine bar2
    use foo2
    class(*), allocatable :: arr(:)
    call store1(arr, [1, 2, 3])  ! SEGFAULT
    select type (a => arr)
      type is (integer)
        if (all (a .eq. [1, 2, 3])) alloc_counts = alloc_counts + 1
    end select
    deallocate (arr)
    call store2(arr, [1, 2, 3])  ! NO PROBLEM
    select type (a => arr)
      type is (integer)
        if (all (a .eq. [1, 2, 3])) alloc_counts = alloc_counts + 1
    end select
  end subroutine bar2

  subroutine bar3
    use foo3
    type(box) :: b
    integer, allocatable :: arr1(:)
    integer, dimension(0) :: arr2

    allocate(arr1(0))
    call store1(b, arr1)
    call vector_type(b%val)  ! OTHER
    call store2(b, arr1)
    call vector_type(b%val)  ! OTHER

    call store1(b, arr2)
    call vector_type(b%val)  ! OTHER
    call store2(b, arr2)
    call vector_type(b%val)  ! OTHER
  end subroutine bar3
end program
