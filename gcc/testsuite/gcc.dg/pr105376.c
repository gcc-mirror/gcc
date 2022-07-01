/* { dg-do compile { target dfp } } */
/* { dg-options "-O -g" } */

void
foo (_Decimal64 d, _Decimal64 e)
{
  d -= -d;
  d *= -e;
}
