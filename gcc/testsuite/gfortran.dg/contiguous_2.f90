! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/40632
!
! CONTIGUOUS compile-time tests
!

integer, pointer, contiguous :: a(:) ! { dg-error "Fortran 2008:" }
integer, pointer :: b(:)
contiguous :: b ! { dg-error "Fortran 2008:" }
end
