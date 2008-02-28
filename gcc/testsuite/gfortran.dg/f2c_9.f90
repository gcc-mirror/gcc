! { dg-do run }
! { dg-options "-ff2c" }
! PR 34868

function f(a) result(res)
  implicit none
  real(8), intent(in) :: a(:)
  complex(8) :: res

  res = cmplx(sum(a),product(a),8)
end function f

function g(a)
  implicit none
  real(8), intent(in) :: a(:)
  complex(8) :: g

  g = cmplx(sum(a),product(a),8)
end function g

program test
  real(8) :: a(1,5)
  complex(8) :: c
  integer :: i

  interface
    complex(8) function f(a)
      real(8), intent(in) :: a(:)
    end function f
    function g(a) result(res)
      real(8), intent(in) :: a(:)
      complex(8) :: res
    end function g
  end interface

  do i = 1, 5
    a(1,i) = sqrt(real(i,kind(a)))
  end do

  c = f(a(1,:))
  call check (real(c), sum(a))
  call check (imag(c), product(a))

  c = g(a(1,:))
  call check (real(c), sum(a))
  call check (imag(c), product(a))
contains
  subroutine check (a, b)
    real(8), intent(in) :: a, b
    if (abs(a - b) > 1.e-10_8) call abort
  end subroutine check
end program test
