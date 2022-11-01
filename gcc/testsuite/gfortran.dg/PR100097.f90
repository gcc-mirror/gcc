! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR100097
!

program main_p
  implicit none

  class(*), pointer     :: bar_p(:)
  class(*), allocatable :: bar_a(:)

  call foo_p(bar_p)
  call foo_a(bar_a)

contains

  subroutine foo_p(that)
    class(*), pointer, intent(out) :: that(..)

    select rank(that)
    rank(1)
    rank default
      stop 1
    end select
  end subroutine foo_p

  subroutine foo_a(that)
    class(*), allocatable, intent(out) :: that(..)

    select rank(that)
    rank(1)
    rank default
      stop 2
    end select
  end subroutine foo_a

end program main_p

! { dg-final { scan-tree-dump "bar_a._data.dtype = \\{.* .rank=1,.*\\}" "original" } }
! { dg-final { scan-tree-dump "bar_p._data.dtype = \\{.* .rank=1,.*\\}" "original" } }
