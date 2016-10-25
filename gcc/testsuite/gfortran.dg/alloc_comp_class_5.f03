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

  if (size(a_list) /= 4) call abort()
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
          if (any(x /= [1, 2])) call abort()
        type is (real(kind=8))
          if (any(x /= [3.0_8, 4.0_8])) call abort()
        type is (logical)
          if (any(x .neqv. [.true., .false.])) call abort()
        type is (character(len=*))
          if (len(x) /= 3) call abort()
          if (any(x /= ["foo", "bar", "baz"])) call abort()
        class default
          call abort()
      end select
    else
        call abort()
    end if
  end subroutine
end
