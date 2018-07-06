! Some basic testing that calls to the library still work correctly with
! -ff2c
!
! Once the library has support for f2c calling conventions (i.e. passing
! a REAL(kind=4) or COMPLEX-valued intrinsic as procedure argument works), we
! can simply add -ff2c to the list of options to cycle through, and get
! complete coverage.  As of 2005-03-05 this doesn't work.
! { dg-do run }
! { dg-options "-ff2c" }

complex c
double complex d

x = 2.
if ((sqrt(x) - 1.41)**2 > 1.e-4) STOP 1
x = 1.
if ((atan(x) - 3.14/4) ** 2 > 1.e-4) STOP 2
c = (-1.,0.)
if (sqrt(c) /= (0., 1.)) STOP 3
d = c
if (sqrt(d) /= (0._8, 1._8)) STOP 4
end
 
