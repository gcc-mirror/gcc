/* Test that imaginary constants are diagnosed in C23 mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic" } */

_Complex float a = 1.if;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex float b = 2.Fj;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex float c = 3.fI;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex float d = 4.JF;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double e = 1.i;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double f = 2.j;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double g = 3.I;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double h = 4.J;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double i = 1.il;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double j = 2.Lj;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double k = 3.lI;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double l = 4.JL;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex float m = 1.if;
__extension__ _Complex float n = 2.Fj;
__extension__ _Complex float o = 3.fI;
__extension__ _Complex float p = 4.JF;
__extension__ _Complex double q = 1.i;
__extension__ _Complex double r = 2.j;
__extension__ _Complex double s = 3.I;
__extension__ _Complex double t = 4.J;
__extension__ _Complex long double u = 1.il;
__extension__ _Complex long double v = 2.Lj;
__extension__ _Complex long double w = 3.lI;
__extension__ _Complex long double x = 4.JL;

int
main ()
{
  if (a * a != -1.f
      || b * b != -4.f
      || c * c != -9.f
      || d * d != -16.f
      || e * e != -1.
      || f * f != -4.
      || g * g != -9.
      || h * h != -16.
      || i * i != -1.L
      || j * j != -4.L
      || k * k != -9.L
      || l * l != -16.L
      || m * m != -1.f
      || n * n != -4.f
      || o * o != -9.f
      || p * p != -16.f
      || q * q != -1.
      || r * r != -4.
      || s * s != -9.
      || t * t != -16.
      || u * u != -1.L
      || v * v != -4.L
      || w * w != -9.L
      || x * x != -16.L)
    __builtin_abort ();
}
