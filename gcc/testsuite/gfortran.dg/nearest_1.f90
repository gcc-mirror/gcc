! { dg-do run }
! { dg-options "-O0 -ffloat-store" }
! { dg-add-options ieee }
! PR fortran/27021
! Original code submitted by Dominique d'Humieres
! Converted to Dejagnu for the testsuite by Steven G. Kargl
program chop
  integer ix, iy
  real o, t, td, tu, x, y
  o = 1.
  t = tiny(o)
  td = nearest(t,-1.0)
  x = td/2.0
  y = nearest(tiny(o),-1.0)/2.0
  ix = transfer(x,ix)
  iy = transfer(y,iy)
  if (ix /= iy) STOP 1
end program chop

