/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-options "-O2 -march=i686" } */
/* { dg-final { scan-assembler "fldpi" } } */

long double atanl (long double);

long double pi()
{
  return 4.0 * atanl (1.0);
}

