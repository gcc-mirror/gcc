! PR fortran/72744
! { dg-do compile }
! { dg-additional-options "-Ofast" }

program pr72744
  integer, parameter :: n = 20
  integer :: i, z(n), h(n)
  z = [(i, i=1,n)]
  h = [(i, i=n,1,-1)]
  call sub (n, h)
  if ( any(h/=z) ) STOP 1
end
subroutine sub (n, x)
  integer :: n, x(n)
!$omp parallel
  x(:) = x(n:1:-1)
!$omp end parallel
end
