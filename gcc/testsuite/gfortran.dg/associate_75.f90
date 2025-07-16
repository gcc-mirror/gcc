! { dg-do run }
!
! Test fix for PR121060.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module subdomain_m
  implicit none

  type subdomain_t 
    real :: s_ = 99.
  contains
    generic :: operator(.laplacian.) => laplacian
    procedure laplacian
  end type

contains

  function laplacian(rhs)
    class(subdomain_t), intent(in) :: rhs
    type(subdomain_t) laplacian
    laplacian%s_ = rhs%s_ + 42
  end function

end module

  use subdomain_m
  implicit none

  type operands_t
    real :: s_
  end type

  type(subdomain_t) phi
  type(operands_t) operands

  associate(laplacian_phi => .laplacian. phi) ! ICE because specific not found.
    operands = approximates(laplacian_phi%s_)
  end associate

  if (int (operands%s_) /= 42) stop 1
contains

  function approximates(actual)
    real actual 
    type(operands_t) approximates
    approximates%s_ = actual - 99
  end function

end
