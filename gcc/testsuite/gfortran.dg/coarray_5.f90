! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Coarray support -- corank declarations
! PR fortran/18918
!

integer :: a, b[*]  ! { dg-error "Fortran 2008: Coarray declaration" }
codimension :: a[*] ! { dg-error "Fortran 2008: Coarray declaration" }
end
