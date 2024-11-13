/* Test that imaginary constants are diagnosed in C23 mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

_Complex _Float128 a = 1.if128;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float128 b = 2.F128j;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float128 c = 3.f128i;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float128 d = 4.JF128;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex _Float128 e = 1.if128;
__extension__ _Complex _Float128 f = 2.F128j;
__extension__ _Complex _Float128 g = 3.f128i;
__extension__ _Complex _Float128 h = 4.JF128;

int
main ()
{
  if (a * a != -1.f128
      || b * b != -4.f128
      || c * c != -9.f128
      || d * d != -16.f128
      || e * e != -1.f128
      || f * f != -4.f128
      || g * g != -9.f128
      || h * h != -16.f128)
    __builtin_abort ();
}
