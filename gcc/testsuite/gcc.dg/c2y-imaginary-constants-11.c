/* Test that integral imaginary constants are diagnosed in C2Y mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=gnu2y -pedantic" } */

_Complex float a = 1i;		/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex float b = 2j;		/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex float c = 3I;		/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex float d = 4J;		/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex double e = 1il;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex double f = 2Lj;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex double g = 3lI;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex double h = 4JL;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex long double i = 1ill;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex long double j = 2LLj;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex long double k = 3llI;	/* { dg-warning "imaginary constants are a GCC extension" } */
_Complex long double l = 4JLL;	/* { dg-warning "imaginary constants are a GCC extension" } */
__extension__ _Complex float m = 1i;
__extension__ _Complex float n = 2j;
__extension__ _Complex float o = 3I;
__extension__ _Complex float p = 4J;
__extension__ _Complex double q = 1il;
__extension__ _Complex double r = 2Lj;
__extension__ _Complex double s = 3lI;
__extension__ _Complex double t = 4JL;
__extension__ _Complex long double u = 1ill;
__extension__ _Complex long double v = 2LLj;
__extension__ _Complex long double w = 3llI;
__extension__ _Complex long double x = 4JLL;

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
