! { dg-do run }
! { dg-options "-fbounds-check" }

program test
  integer x(20)
  integer, volatile :: n
  n = 1
  if (size(x(n:2:-3)) /= 0) STOP 1

  call ha0020(-3)
  call ha0020(-1)
end program test

subroutine ha0020(mf3)
  implicit none
  integer xca(2), xda(2), mf3

  xca = 1
  xda = -1

  xca(1:2:-1) = xda(1:2:mf3)

  if (any (xca /= 1)) STOP 2
  if (any(xda(1:2:mf3) /= xda(1:0))) STOP 3
  if (size(xda(1:2:mf3)) /= 0) STOP 4
  if (any(shape(xda(1:2:mf3)) /= 0)) STOP 5
  if (any(ubound(xda(1:2:mf3)) /= 0)) STOP 6
  if (ubound(xda(1:2:mf3),1) /= 0) STOP 7
  if (lbound(xda(1:2:mf3),1) /= 1) STOP 8

end subroutine
