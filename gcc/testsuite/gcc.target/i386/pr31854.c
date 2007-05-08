/* { dg-do compile { target dfp } } */
/* { dg-options "-O -std=gnu99" } */

_Decimal128 d128;
long double tf;

void foo (void)
{
  d128 = tf;
}
