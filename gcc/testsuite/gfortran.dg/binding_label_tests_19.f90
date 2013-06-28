! { dg-do compile }
!
! PR fortran/48858
!
subroutine foo() bind(C,name="bar")
end subroutine foo

subroutine foo() bind(C,name="sub")
end subroutine foo

