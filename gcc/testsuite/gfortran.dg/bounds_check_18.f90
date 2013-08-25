! { dg-do compile }
program main
  implicit none
  integer :: n
  real, dimension(10) :: a
  n = 0
  call random_number(a)
  if (any(a(n+1:n+5) > [1.0, 2.0, 3.0])) print *,"Hello!" ! { dg-error "not conformable" }
end program main
