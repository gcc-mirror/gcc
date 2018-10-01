! { dg-do compile }
!
! PR 86830: [8/9 Regression] Contiguous array pointer function result not recognized as contiguous
!
! Contributed by <only_for_nouse@gmx.de>

module m
  implicit none

  type :: t1
   contains
     procedure :: get_ptr
  end type

  type :: t2
     class(t1), allocatable :: c
  end type

contains

  function get_ptr(this)
    class(t1) :: this
    real, dimension(:), contiguous, pointer :: get_ptr
  end function

  subroutine test()
    real, dimension(:), contiguous, pointer:: ptr
    type(t2) :: x
    ptr => x%c%get_ptr()
  end subroutine

end module
