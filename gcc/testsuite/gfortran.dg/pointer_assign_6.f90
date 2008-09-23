! { dg-do compile }
! { dg-options "-std=f95" }
! PR fortran/37580
!
program test
implicit none
real, pointer :: ptr1(:), ptr2(:)
ptr1(1:) => ptr2 ! { dg-error "Fortran 2003: Bounds specification" }
end program test
