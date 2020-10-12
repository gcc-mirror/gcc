/* Verify that 387 fsincos instruction is generated.  */
/* { dg-do compile } */
/* { dg-options "-O -ffast-math -mfpmath=387 -mfancy-math-387" } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-final { scan-assembler "fsincos" } } */

extern double sin (double);
extern double cos (double);

double f1(double x)
{
  return sin(x) + cos (x);
}

