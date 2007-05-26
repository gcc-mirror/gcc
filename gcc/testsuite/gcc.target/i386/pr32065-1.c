/* { dg-do compile { target dfp } } */
/* { dg-options "-msse -std=gnu99" } */

_Decimal128 test (void)
{
  return 1234123412341234.123412341234dl;
}
