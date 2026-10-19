! { dg-do run }
! PR 127671
! Span of a TARGET dummy referenced only from a contained procedure.

module m
  implicit none
  type t
     integer :: n = 0
  end type t
  type w
     real :: pad(3)
     type(t) :: x
  end type w
contains
  subroutine host (a, s, s2)
    type(t), intent(in), target :: a(:)
    integer, intent(out) :: s, s2
    s = 0
    call inner ()
    s2 = sum (a%n)
  contains
    subroutine inner ()
      integer :: i
      do i = 1, size (a)
        s = s + a(i)%n * i
      end do
    end subroutine inner
  end subroutine host
end module m
program p
  use m
  implicit none
  type(w) :: v(6)
  integer :: i, s, s2
  do i = 1, 6
    v(i)%x%n = 10 * i
  end do
  call host (v(::2)%x, s, s2)
  if (s /= 10*1 + 30*2 + 50*3) stop 1
  if (s2 /= 90) stop 2
  call host (v%x, s, s2)
  if (s2 /= 210) stop 3
end program p
