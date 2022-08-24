/* { dg-do compile { target dfp } } */
/* { dg-options "-Ofast" } */

int i;

int
foo(char c, _Decimal32 d)
{
  d *= i;
  d *= -(_Decimal64)c;
  return d;
}
