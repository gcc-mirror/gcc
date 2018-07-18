! { dg-do run }
! { dg-options "-std=gnu" }
  integer :: x(9), y(9), t

  t = time()
  call ltime(t,x)
  call gmtime(t,y)
  if (x(1) /= y(1) .or. mod(x(2),30) /= mod(y(2),30)) STOP 1
  end
