! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Fortran 2003 allowes TYPE without components
! The error message for -std=f95 is tested in
! gfortran.dg/access_spec_2.f90
!
! PR fortran/33188
!
type t
end type

type(t) :: a
print *, a
end
