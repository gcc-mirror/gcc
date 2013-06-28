! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/48858
!
subroutine foo() bind(C,name="bar") ! { dg-error "Global name 'foo' at .1. is already being used as a SUBROUTINE at .2." }
end subroutine foo

subroutine foo() bind(C,name="sub") ! { dg-error "Global name 'foo' at .1. is already being used as a SUBROUTINE at .2." }
end subroutine foo

