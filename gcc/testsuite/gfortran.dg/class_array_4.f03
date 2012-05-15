! { dg-do run }
! PR43214 - implementation of class arrays
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module m
  type t
    real :: r = 99
  contains
    procedure, pass :: foo => foo
  end type t
contains
  elemental subroutine foo(x, i)
    class(t),intent(in) :: x
    integer,intent(inout) :: i
    i = x%r + i
  end subroutine foo
end module m

  use m
  type(t) :: x(3)
  integer :: n(3) = [0,100,200]
  call x(:)%foo(n)
  if (any(n .ne. [99,199,299])) call abort
end
