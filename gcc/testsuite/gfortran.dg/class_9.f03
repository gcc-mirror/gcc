! { dg-do run }
! Test the fix for PR41706, in which arguments of class methods that
! were themselves class methods did not work.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>
!
module m
type :: t
  real :: v = 1.5
contains
  procedure, nopass :: a
  procedure, nopass :: b
  procedure, pass :: c
  procedure, nopass :: d
end type

contains

  real function a (x)
    real :: x
    a = 2.*x
  end function

  real function b (x)
    real :: x
    b = 3.*x
  end function

  real function c (x)
    class (t) :: x
    c = 4.*x%v
  end function

  subroutine d (x)
    real :: x
    if (abs(x-3.0)>1E-3) call abort()
  end subroutine

  subroutine s (x)
    class(t) :: x
    real :: r
    r = x%a (1.1)       ! worked
    if (r .ne. a (1.1)) call abort

    r = x%a (b (1.2))   ! worked
    if (r .ne. a(b (1.2))) call abort

    r = b ( x%a (1.3))  ! worked
    if (r .ne. b(a (1.3))) call abort

    r = x%a(x%b (1.4))   ! failed
    if (r .ne. a(b (1.4))) call abort

    r = x%a(x%c ())   ! failed
    if (r .ne. a(c (x))) call abort

    call x%d (x%a(1.5))  ! failed

  end subroutine

end

  use m
  class(t),allocatable :: x
  allocate(x)
  call s (x)
end
! { dg-final { cleanup-modules "m" } }
