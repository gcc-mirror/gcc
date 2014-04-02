/* { dg-do compile } */
/* { dg-options "-O2 -mfancy-math-387 -mtune=generic" } */
/* { dg-final { scan-assembler "fldpi" } } */
/* { dg-require-effective-target large_long_double } */

long double atanl (long double);

long double pi()
{
  return 4.0 * atanl (1.0);
}

