! { dg-do run }
! { dg-additional-options "-ffrontend-optimize -Warray-temporaries" }
! PR 93113 - this used to ICE, and should not generate a temporary.
program main
  integer, parameter :: n = 10
  complex, dimension(n,n) :: a, b, c
  real, dimension(n,n) :: r
  call random_number (r)
  c%re = r
  call random_number (r)
  c%im = r

  a = c
  b = c
  b%re = a%re - 0.5
  b%im = a%im - 0.5
  a%re = a%re - 0.5
  a%im = a%im - 0.5
  if (any (a /= b)) stop 1
  b%im = a%re
  a%im = a%re
  if (any (a /= b)) stop 2
  a = c
  b = c
  b(2:n,:)%re = a(1:n-1,:)%re
  a(2:n,:)%re = a(1:n-1,:)%re
  if (any (a /= b)) stop 3
  a = c
  b = c
  b(1:n-1,:)%im = a(2:,:)%im
  a(1:n-1,:)%im = a(2:,:)%im
  if (any (a /= b)) stop 3
end program main
