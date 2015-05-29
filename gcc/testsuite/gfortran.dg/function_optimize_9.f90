! { dg-do compile }
! { dg-options "-O -fdump-tree-original" }
program main
  integer, parameter :: n=100
  real, parameter :: pi=4*atan(1.)
  real, parameter :: tmax=20.
  real, parameter :: dt = tmax/(2*pi)/real(n)
  real, parameter :: t0 = dt/30.
  integer :: i
  interface
     pure function purefunc(x)
       real :: purefunc
       real, intent(in) :: x
     end function purefunc
  end interface
  real :: a(n)
  do i=1,n
     a(i) = purefunc(dt*i + t0) * 3. + 2 * purefunc(t0 + i*dt) 
  end do
  print *,a
end program main
! { dg-final { scan-tree-dump-times "purefunc" 1 "original" } }
