! { dg-do run }
!
! PR fortran/51218
!
! Contributed by Harald Anlauf
!

module a
  implicit none
  integer :: neval = 0
contains
  subroutine inc_eval
    neval = neval + 1
  end subroutine inc_eval
end module a

module b
  use a
  implicit none
contains
  function f(x) ! Should be implicit pure
    real :: f
    real, intent(in) :: x
    f = x
  end function f

  function g(x) ! Should NOT be implicit pure
    real :: g
    real, intent(in) :: x
    call inc_eval
    g = x
  end function g
end module b

program gfcbug114a
  use a
  use b
  implicit none
  real :: x = 1, y = 1, t, u, v, w
  if (neval /= 0) STOP 1
  t = f(x)*f(y)
  if (neval /= 0) STOP 2
  u = f(x)*f(y) + f(x)*f(y)
  if (neval /= 0) STOP 3
  v = g(x)*g(y)
  if (neval /= 2) STOP 4
  w = g(x)*g(y) + g(x)*g(y)
  if (neval /= 6) STOP 5
  if (t /= 1.0 .or. u /= 2.0 .or. v /= 1.0 .or. w /= 2) STOP 6
end program gfcbug114a

! { dg-final { scan-module "b" "IMPLICIT_PURE" } }
