! { dg-do run }
! { dg-additional-sources f2c_4.c }
! { dg-options "-ff2c -w" }

! Check -ff2c calling conventions
!   Return value of REAL function is promoted to C type double
!   Return value of COMPLEX function is via an extra argument in the
!    calling sequence that points to where to store the return value
!   Addional underscore appended to function name
program f2c_4
  complex c, f2c_4k, f2c_4l
  double complex z, f2c_4m, f2c_4n
  integer i

  ! Promotion of REAL function
  call f2c_4a()

  ! Return COMPLEX arg - call Fortran routines from C
  call f2c_4c()
  call f2c_4e()
  call f2c_4g()
  call f2c_4i()

  !  Return COMPLEX arg - call C routines from Fortran 
  c = cmplx(1234.0,5678.0)
  z = dcmplx(1234.0d0,5678.0d0)
  if ( c .ne. f2c_4k(c) )   call abort
  if ( c .ne. f2c_4l(i,c) ) call abort
  if ( z .ne. f2c_4m(z) )   call abort
  if ( z .ne. f2c_4n(i,z) ) call abort

end

real function f2c_4b(x)
  double precision x
  f2c_4b = x
end

complex function f2c_4d(x)
  complex x
  f2c_4d = x
end

complex function f2c_4f(i,x)
  complex x
  integer i
  f2c_4f = x
end

double complex function f2c_4h(x)
  double complex x
  f2c_4h = x
end

double complex function f2c_4j(i,x)
  double complex x
  f2c_4j = x
end
