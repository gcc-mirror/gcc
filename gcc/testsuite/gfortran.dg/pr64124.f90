! { dg-do compile }
! PR fortran/64124.f90
  character(len=kind(1)) x
  integer(len(x)) y
  end
