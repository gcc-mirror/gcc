! { dg-do compile }
!
! Check conflicts:
! - PARAMETER and BIND(C), PR fortran/33310
! - INTRINSIC and ENTRY, PR fortran/33284
!

subroutine a
 intrinsic  cos
entry cos(x) ! { dg-error "ENTRY attribute conflicts with INTRINSIC" }
 real x
 x = 0
end subroutine

module m
    use iso_c_binding
    implicit none
    TYPE, bind(C) :: the_distribution
        INTEGER(c_int) :: parameters(1)
    END TYPE the_distribution
    TYPE (the_distribution), parameter, bind(C) :: & ! { dg-error "PARAMETER attribute conflicts with BIND.C." }
                          the_beta = the_distribution((/0/))
end module m

end
