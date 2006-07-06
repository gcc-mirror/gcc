subroutine foo(n,x)
  implicit none
  integer, intent(in) :: n
  complex(8), intent(out) :: x(n,*)
  x(1,1) = 0.d0
  x(n,1) = 0.d0
  x(:,1) = 0.d0
  x(2:,1) = 0.d0
  x(:n-1,1) = 0.d0
  x((/1,n/),1) = 0.d0
end subroutine foo

program test
  implicit none
  integer, parameter :: n = 17
  complex(8) :: x(n,n)
  call foo(n,x)
end program test
