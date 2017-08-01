! { dg-do compile }
!
! PR53542 USE-associated variables shows original instead of renamed symbol name
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module select_precision
    integer :: dp = kind(1.0)
end module select_precision

module ode_types
    use select_precision, only: wp => dp
contains
    subroutine ode_derivative(x)
        real(wp) :: x ! { dg-error "Parameter .wp. at .1. has not been declared" }
    end subroutine ode_derivative
end module ode_types
end
