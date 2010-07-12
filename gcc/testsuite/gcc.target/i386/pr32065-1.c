/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target sse } */
/* { dg-options "-msse -std=gnu99" } */

_Decimal128 test (void)
{
  return 1234123412341234.123412341234dl;
}
