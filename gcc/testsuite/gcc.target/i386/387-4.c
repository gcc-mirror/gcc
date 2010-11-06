/* { dg-do compile } */
/* { dg-options "-O2 -mfancy-math-387" } */
/* { dg-final { scan-assembler "fldpi" } } */

long double atanl (long double);

long double pi()
{
  return 4.0 * atanl (1.0);
}

