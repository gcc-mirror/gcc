! { dg-do compile }
! PR fortran/56520
!
program misleading
    implicit none
    real a, c
    a = 1.0
    c = exp(+a) )    ! { dg-error "Unclassifiable statement" }
    c = exp(-a) )
    c = exp((a)) )
    c = exp(a) )
    c = exp(a)
end program misleading
