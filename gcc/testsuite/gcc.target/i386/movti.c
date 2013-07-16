/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O -std=gnu99" } */

_Decimal128 test (void)
{
  return 1234123412341234.123412341234dl;
}

/* { dg-final { scan-assembler-not "movabs" { target { ! x86_64-*-mingw* } } } } */
