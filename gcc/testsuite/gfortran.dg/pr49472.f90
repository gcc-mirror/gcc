! PR rtl-optimization/49472
! { dg-do compile }
! { dg-options "-O -fcompare-debug -ffast-math" }
subroutine pr49472
  integer, parameter :: n = 3
  real(8) :: a, b, c, d, e (n+1)
  integer :: i
  do i=2, (n+1)
    b = 1. / ((i - 1.5d0) * 1.)
    c = b * a
    d = -b * c / (1. + b * b) ** 1.5d0
    e(i) = d
  end do
  call dummy (e)
end subroutine
