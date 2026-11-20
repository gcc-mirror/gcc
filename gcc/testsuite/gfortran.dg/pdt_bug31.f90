! { dg-do run }
!
! CLASS PDT dummy arrays: Primary problem was a repeated typo in trans-decl.cc.
!
! Subroutine 's_out' needed special attention because the vptr default initializer
! was NULL.
!
! Contributed by Jerry Delisle  <jvdelisle@gcc.gnu.org>
!
module m
  implicit none
  type t(n)
    integer, len :: n = 2
    integer :: x(n)
    integer :: tag = 42
  end type
contains
  subroutine s_shape (a)
    class(t(n=3)), intent(in) :: a(:)
    if (sum (a%x(2)) /= 7) stop 1
  end subroutine
  subroutine s_expl (a)
    class(t(n=3)), intent(in) :: a(2)
    if (sum (a(1)%x) /= 6) stop 2
  end subroutine
  subroutine s_size (a)
    class(t(n=3)), intent(in) :: a(*)
    if (sum (a(2)%x) /= 15) stop 3
  end subroutine
  subroutine s_rank (a, i)
    class(t(n=3)), intent(in) :: a(..)
    integer, intent(in) :: i
    if (rank (a) /= i) stop 4
  end subroutine
  subroutine s_alloc (a)
    class(t(n=:)), allocatable, intent(inout) :: a(:)
    if (.not. allocated (a)) allocate (t(n=3) :: a(2))
    a(1)%x = [11, 12, 13]
    a(2)%x = [14, 15, 16]
  end subroutine
  subroutine s_out (a)
    class(t(n=3)), intent(out) :: a(:)
    if ((size (a, 1) /= 2) .or. &            ! Dimension of a retained
        (sum (a(1)%x) /= 36) .or. &          ! Size and contents of pdt_array retained
        a(2)%tag /= 42) stop 5               ! Not pdt component set to initalizer.
  end subroutine
end module

  use m
  class(t(n=3)), allocatable :: x(:), y(:,:)
  allocate (x(2))
  x(1)%x = [1, 2, 3]
  x(2)%x = [4, 5, 6]
  x%tag = 1
  call s_shape (x)
  call s_expl (x)
  call s_size (x)
  call s_rank (x, 1)
  call s_rank (y, 2)
  deallocate (x)
  call s_alloc (x)
  if (sum (x(1)%x) + sum (x(2)%x) /= 81) stop 6
  x%tag = 1
  call s_out (x)
  if (allocated (x)) deallocate (x)
end
