! Make sure the f2c calling conventions work
! { dg-do run }
! { dg-options "-ff2c" }

function f(x)
  f = x
end function f

complex function c(a,b)
  c = cmplx (a,b)
end function c

double complex function d(e,f)
  double precision e, f
  d = cmplx (e, f, kind(d))
end function d

subroutine test_with_interface()
  interface
     real function f(x)
       real::x
     end function f
  end interface

  interface
     complex function c(a,b)
       real::a,b
     end function c
  end interface

  interface
     double complex function d(e,f)
       double precision::e,f
     end function d
  end interface
  
  double precision z, w

  x = 8.625
  if (x /= f(x)) call abort ()
  y = f(x)
  if (x /= y) call abort ()

  a = 1.
  b = -1.
  if (c(a,b) /= cmplx(a,b)) call abort ()

  z = 1.
  w = -1.
  if (d(z,w) /= cmplx(z,w, kind(z))) call abort ()
end subroutine test_with_interface

external f, c, d
real f
complex c
double complex d
double precision z, w

x = 8.625
if (x /= f(x)) call abort ()
y = f(x)
if (x /= y) call abort ()

a = 1.
b = -1.
if (c(a,b) /= cmplx(a,b)) call abort ()

z = 1.
w = -1.
if (d(z,w) /= cmplx(z,w, kind(z))) call abort ()

call test_with_interface ()
end
