/* Verify that 387 fptan instruction is generated. Also check that
   inherent load of 1.0 is used in further calculations.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mfpmath=387 -mfancy-math-387" } */
/* { dg-final { scan-assembler "fptan" } } */
/* { dg-final { scan-assembler-not "fld1" } } */

extern double tan (double);

double f1(double x)
{
  return 1.0 / tan(x);
}

