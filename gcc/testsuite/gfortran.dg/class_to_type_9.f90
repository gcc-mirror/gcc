! { dg-do run }
! PR fortran/53800

! A TARGET dummy associated with elements that are spaced by more than the
! element size: pointers to it stay valid after the call, it is written
! through, it is not contiguous, it is copied when passed on to a dummy
! without the TARGET attribute and it is transferred element by element.

module m
  implicit none
  type :: t
    integer :: c1, c2
  end type
  integer, pointer :: saved(:) => null()
contains
  subroutine chk(a, opt)
    integer, target :: a(:)
    integer, optional, target :: opt(:)
    character(24) :: line
    if (is_contiguous(a)) stop 1
    if (present(opt)) stop 2
    write (line, '(4I3)') a
    if (line(1:12) /= '  1  4  9 16') stop 3
    if (sum(a) /= 30) stop 4
    call packed(a)
    call assumed_size(a)
    saved => a
    a(2) = -a(2)
  end subroutine
  subroutine packed(b)         ! copy-in/copy-out
    integer :: b(:)
    if (any(b /= [1, 4, 9, 16])) stop 5
    if (.not. is_contiguous(b)) stop 6
  end subroutine
  subroutine assumed_size(c)   ! no descriptor
    integer :: c(*)
    if (any(c(1:4) /= [1, 4, 9, 16])) stop 7
  end subroutine
  subroutine rank_any(d)
    integer, target :: d(..)
    select rank (d)
    rank (1)
      if (any(d /= [1, 2, 3, 4])) stop 8
      saved => d
    rank default
      stop 9
    end select
  end subroutine
end module

program p
  use m
  implicit none
  type(t), target :: x(4)
  integer :: i
  x = [(t(i, i*i), i=1,4)]

  call chk(x%c2)
  if (any(x%c2 /= [1, -4, 9, 16])) stop 10
  saved = 0
  if (any(x%c2 /= [0, 0, 0, 0])) stop 11
  if (any(x%c1 /= [1, 2, 3, 4])) stop 12

  call rank_any(x%c1)
  saved = 7
  if (any(x%c1 /= [7, 7, 7, 7])) stop 13
end program
