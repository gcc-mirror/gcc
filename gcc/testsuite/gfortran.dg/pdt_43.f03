! { dg-do run )
!
! Test the fix for PR89707 in which the procedure pointer component
! with a parameterized KIND expression caused an ICE in resolution.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
program pdt_with_ppc
  integer, parameter :: kt = kind (0d0)
  type :: q(k)
     integer, kind :: k = 4
     procedure (real(kind=kt)), pointer, nopass :: p
  end type
  type (q(kt)) :: x
  x%p => foo
  if (int (x%p(2d0)) /= 4) stop 1
  x%p => bar
  if (int (x%p(2d0, 4d0)) /= 16) stop 2
contains
  real(kind=kt) function foo (x)
    real(kind = kt) :: x
    foo = 2.0 * x
  end
  real(kind=kt) function bar (x, y)
    real(kind = kt) :: x, y
    bar = x ** y
  end
end
