! { dg-do run }
!
! PR fortran/126895
! The IS_CONTIGUOUS intrinsic used to erroneously return FALSE for pointer
! assumed-rank dummies associated with a scalar value.

program prog
  implicit none
  type :: t
    integer :: c1
  end type
  type, extends(t) :: u
    integer :: c2
  end type
  type(t), target :: x
  type(u), target :: y
  call s1(x, 1)
  call s1(y, 2)
contains
  subroutine s1(a, e)
    class(t), pointer, intent(in) :: a(..)
    integer, value :: e
    !print *, is_contiguous(a)
    if (.not. is_contiguous(a)) error stop e
  end subroutine
end program
