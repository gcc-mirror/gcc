/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mavx -std=gnu99" } */

_Decimal128
foo128 (_Decimal128 z)
{
  return z + 1.0dl;
}
