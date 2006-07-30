! { dg-do run }
! { dg-options "-std=gnu" }
  integer :: x(9), y(9), t

  t = time()
  call ltime(t,x)
  call gmtime(t,y)
  if (x(1) /= y(1) .or. x(2) /= y(2)) call abort
  end
