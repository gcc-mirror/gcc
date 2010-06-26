! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Fortran 2008: TYPE ( intrinsic-type-spec )
!
implicit none
type(integer)          :: a ! { dg-error "Fortran 2008" }
type(real)             :: b ! { dg-error "Fortran 2008" }
type(logical)          :: c ! { dg-error "Fortran 2008" }
type(character)        :: d ! { dg-error "Fortran 2008" }
type(double precision) :: e ! { dg-error "Fortran 2008" }
end
