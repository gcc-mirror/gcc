! { dg-do compile }
!
! PR fortran/53537
! The use of WP in the ODE_DERIVATIVE interface used to be rejected because
! the symbol was imported under the original name DP.
!
! Original test case from Arjen Markus <arjen.markus@deltares.nl>

module select_precision
    integer, parameter :: dp = kind(1.0)
end module select_precision

module ode_types
    use select_precision, only: wp => dp
    implicit none
    interface
        subroutine ode_derivative(x)
            import   :: wp
            real(wp) :: x
        end subroutine ode_derivative
    end interface
end module ode_types


