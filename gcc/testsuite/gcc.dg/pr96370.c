/* { dg-do compile { target dfp } } */
/* { dg-options "-O2 -ffast-math" } */

void c(_Decimal128);
void a(_Decimal128 b)
{
  c(-b * b);
}
