! { dg-do compile }
! PR fortran/37580
!
program test
implicit none
real, pointer :: ptr1(:), ptr2(:)
ptr1(1) => ptr2 ! { dg-error "Expected bounds specification" }
ptr1(1:) => ptr2 ! { dg-error "not yet implemented in gfortran" }
end program test
