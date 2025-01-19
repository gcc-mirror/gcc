/* Test that imaginary constants are diagnosed in C23 mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

_Complex _Float64x a = 1.if64x;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float64x b = 2.F64xj;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float64x c = 3.f64xi;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float64x d = 4.JF64x;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex _Float64x e = 1.if64x;
__extension__ _Complex _Float64x f = 2.F64xj;
__extension__ _Complex _Float64x g = 3.f64xi;
__extension__ _Complex _Float64x h = 4.JF64x;

int
main ()
{
  if (a * a != -1.f64x
      || b * b != -4.f64x
      || c * c != -9.f64x
      || d * d != -16.f64x
      || e * e != -1.f64x
      || f * f != -4.f64x
      || g * g != -9.f64x
      || h * h != -16.f64x)
    __builtin_abort ();
}
