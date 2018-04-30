! initialization expression, now allowed in Fortran 2003
! PR fortran/29962
! { dg-do run }
! { dg-options "-std=f2003 " }
  real, parameter :: three = 27.0**(1.0/3.0)
  if(abs(three-3.0)>epsilon(three)) STOP 1
end
