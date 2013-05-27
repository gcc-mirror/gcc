! { dg-do compile }
!
! PR fortran/48858
!
subroutine foo() bind(C, name="bar") ! { dg-error "Global binding name 'bar' at .1. is already being used as a SUBROUTINE at .2." }
end subroutine foo

subroutine sub() bind(C, name="bar") ! { dg-error "Global binding name 'bar' at .1. is already being used as a SUBROUTINE at .2." }
end subroutine sub

