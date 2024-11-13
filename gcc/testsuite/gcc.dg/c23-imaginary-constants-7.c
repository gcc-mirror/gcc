/* Test that imaginary constants are diagnosed in C23 mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Complex _Float16 a = 1.if16;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float16 b = 2.F16j;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float16 c = 3.f16i;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float16 d = 4.JF16;	/* { dg-warning "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex _Float16 e = 1.if16;
__extension__ _Complex _Float16 f = 2.F16j;
__extension__ _Complex _Float16 g = 3.f16i;
__extension__ _Complex _Float16 h = 4.JF16;

int
main ()
{
  if (a * a != -1.f16
      || b * b != -4.f16
      || c * c != -9.f16
      || d * d != -16.f16
      || e * e != -1.f16
      || f * f != -4.f16
      || g * g != -9.f16
      || h * h != -16.f16)
    __builtin_abort ();
}
