! { dg-do compile }
! { dg-options "-std=f95" }
  integer,parameter :: i = 42
  real,parameter :: x = 17.
  complex,parameter :: z = (1.,2.)
  complex,parameter :: c1 = (i, 0.5) ! { dg-error "Fortran 2003: PARAMETER symbol in complex constant" }
  complex,parameter :: c2 = (x, 0.5) ! { dg-error "Fortran 2003: PARAMETER symbol in complex constant" }
  complex,parameter :: c3 = (z, 0.) ! { dg-error "Fortran 2003: PARAMETER symbol in complex constant" }
  print *, c1, c2, c3
  end
