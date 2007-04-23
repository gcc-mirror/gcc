! { dg do-compile }
! Tests the fix for PR31630, in which the association of the argument
! of 'cmp' did not work.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
module box_module
  type box
    integer :: m = 0
  end type box
end module box_module

module sort_box_module
contains

  subroutine heapsort_box(cmp)
    interface
       subroutine cmp(a)
         use box_module
         type(box) :: a
       end subroutine cmp
    end interface
    optional :: cmp
  end subroutine heapsort_box

end module sort_box_module


module boxarray_module
  use box_module
  implicit none

  type boxarray
    type(box), allocatable :: bxs(:)
  end type boxarray
contains

  subroutine boxarray_build_l(ba)
    type(boxarray) :: ba
    allocate(ba%bxs(1))
  end subroutine boxarray_build_l

  subroutine boxarray_sort()
    use sort_box_module
    call heapsort_box
  end subroutine boxarray_sort

end module boxarray_module

! { dg-final { cleanup-modules "box_module sort_box_module boxarray_module" } }
