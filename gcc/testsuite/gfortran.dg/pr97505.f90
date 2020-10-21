! { dg-do compile }
! { dg-options "-Os -fsanitize=signed-integer-overflow" }
!
! Test the fix for PR35824, in which the interface assignment and
! negation did not work correctly.
!
! Contributed by Rolf Roth <everyo@gmx.net>
!
module typemodule
  type alltype
     double precision :: a
     double precision,allocatable :: b(:)
  end type
  interface assignment(=)
    module procedure at_from_at
  end interface
  interface operator(-)
    module procedure  neg_at
  end interface
contains
  subroutine at_from_at(b,a)
    type(alltype), intent(in) :: a
    type(alltype), intent(out) :: b
    b%a=a%a
    allocate(b%b(2))
    b%b=a%b
  end subroutine at_from_at
  function neg_at(a) result(b)
    type(alltype), intent(in) :: a
    type(alltype) :: b
    b%a=-a%a
    allocate(b%b(2))
    b%b=-a%b
  end function neg_at
end module
  use typemodule
  type(alltype) t1,t2,t3
  allocate(t1%b(2))
  t1%a=0.5d0
  t1%b(1)=1d0
  t1%b(2)=2d0
  t2=-t1
  if (t2%a .ne. -0.5d0) STOP 1
  if (any(t2%b .ne. [-1d0, -2d0])) STOP 2

  t1=-t1
  if (t1%a .ne. -0.5d0) STOP 3
  if (any(t1%b .ne. [-1d0, -2d0])) STOP 4
end
