! PR tree-optimization/65450
! { dg-do run }
! { dg-additional-options "-mtune=amdfam10" { target x86_64-*-* i?86-*-* } }

program pr65450
  integer :: n, m, o, i, k
  double precision :: u(500,60,3), h(500,60,3)
  double precision :: v(500,60)
  u = 0
  h = 0
  o = 1
  m = 2
  n = 3
  do k = 1, 50
    v = foo (u(:,:,m))
    u(2:499,1:60,n) = u(2:499,1:60,o)+16.d0
    h(1:500,2:59,n) = h(1:500,2:59,o)-4.d0*v(1:500,2:59)-32.0d0
    i = o
    o = m
    m = n
    n = i
  end do
  if (abs (v(17, 23) + h(17, 23, 2) + 768.0d0) > 0.5d0) call abort
contains
  function foo(a)
    double precision :: a(:,:)
    double precision :: foo(size(a,dim=1),size(a,dim=2))
    integer :: i, j
    i = size(a,dim=1)
    j = size(a,dim=2)
    foo(2:i-1,1:j) = a(3:i,1:j)-a(1:i-2,1:j)
    foo(1,1:j) = 2*(a(2,1:j)-a(1,1:j))
    foo(i,1:j) = 2*(a(i,1:j)-a(i-1,1:j))
  end function foo
end program pr65450
