! { dg-do run }
!
! Contributed by Vladimir Fuka
! Check that pr61337 and pr78053, which was caused by this testcase, is fixed.

module array_list

  type container
    class(*), allocatable :: items(:)
  end type

contains

  subroutine add_item(a, e)
    type(container),allocatable,intent(inout) :: a(:)
    class(*),intent(in) :: e(:)
    type(container),allocatable :: tmp(:)

      if (.not.allocated(a)) then
        allocate(a(1))
        allocate(a(1)%items(size(e)), source = e)
      else
        call move_alloc(a,tmp)
        allocate(a(size(tmp)+1))
        a(1:size(tmp)) = tmp
        allocate(a(size(tmp)+1)%items(size(e)), source=e)
      end if
   end subroutine

end module

program test_pr61337

  use array_list

  type(container), allocatable :: a_list(:)
  integer(kind = 8) :: i

  call add_item(a_list, [1, 2])
  call add_item(a_list, [3.0_8, 4.0_8])
  call add_item(a_list, [.true., .false.])
  call add_item(a_list, ["foo", "bar", "baz"])

  if (size(a_list) /= 4) STOP 1
  do i = 1, size(a_list)
          call checkarr(a_list(i))
  end do

  deallocate(a_list)

contains

  subroutine checkarr(c)
    type(container) :: c

    if (allocated(c%items)) then
      select type (x=>c%items)
        type is (integer)
          if (any(x /= [1, 2])) STOP 2
        type is (real(kind=8))
          if (any(x /= [3.0_8, 4.0_8])) STOP 3
        type is (logical)
          if (any(x .neqv. [.true., .false.])) STOP 4
        type is (character(len=*))
          if (len(x) /= 3) STOP 5
          if (any(x /= ["foo", "bar", "baz"])) STOP 6
        class default
          STOP 7
      end select
    else
        STOP 8
    end if
  end subroutine
end
