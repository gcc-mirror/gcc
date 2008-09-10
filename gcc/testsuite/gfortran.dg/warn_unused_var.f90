! { dg-do compile }
! { dg-options "-Wunused-variable" }
!
! PR fortran/37420
!
integer :: i ! { dg-warning "Unused variable" }
end
