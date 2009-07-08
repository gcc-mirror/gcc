! { dg-do run }
! { dg-options "-fno-sign-zero" }
!
! PR fortran/40675
!
! Fortran 77 just had: "The value of a signed zero is the same as
! the value of an unsigned zero." and g77 returned for SIGN(1.0, -0.0) = 1.0
!
! Fortran 95+ has for SIGN: "Case  (iv):  If B is of type real and is zero,
! then ... (c) If B is negative real zero, the value of the result is -|A|".
! On architectures, where signed zeros are supported, gfortran's SIGN thus
! returns for B=-0.0 the -|A|.
!
program s
   x = sign(1.,0.)
   y = sign(1.,-0.)
   if (x /= 1.) call abort()
   if (y /= 1.) call abort()
   x = 1.
   y = 0.
   x = sign(x, y)
   y = sign(x, -y)
   if (x /= 1.) call abort()
   if (y /= 1.) call abort()
end program s
