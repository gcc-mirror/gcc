! { dg-do compile }
! PR fortran/37580

! See also the pointer_remapping_* tests.

program test
implicit none
real, pointer :: ptr1(:), ptr2(:)
ptr1(1) => ptr2 ! { dg-error "Expected bounds specification" }
end program test
