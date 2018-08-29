! { dg-do run }
!
  call s(1,0)
  call s(2,0)
  call s(3,0)
  call s(4,0)
  call s(5,1)
  call s(6,2)
  call s(7,3)
contains
  subroutine s(n,m)
    implicit none
    integer n, m
    real x(10)
    if (any (lbound(x(5:n)) /= 1)) STOP 1
    if (lbound(x(5:n),1) /= 1) STOP 2
    if (any (ubound(x(5:n)) /= m)) STOP 3
    if (ubound(x(5:n),1) /= m) STOP 4
  end subroutine
end program
