/* Verify that 387 fsincos instruction is generated.  */
/* { dg-do compile } */
/* { dg-options "-O -funsafe-math-optimizations -mfpmath=387 -mfancy-math-387" } */
/* { dg-require-effective-target c99_runtime } */

extern double sin (double);
extern double cos (double);
extern void sincos (double, double *, double *);

double f1(double x)
{
  double s, c;
  sincos (x, &s, &c);
  return s + c;
}

double f2(double x)
{
  double s, c, tmp;
  sincos (x, &s, &tmp);
  c = cos (x);
  return s + c;
}

double f3(double x)
{
  double s, c, tmp;
  sincos (x, &tmp, &c);
  s = sin (x);
  return s + c;
}

/* { dg-final { scan-assembler "fsincos" } } */
/* { dg-final { scan-assembler-not "fsin " } } */
/* { dg-final { scan-assembler-not "fcos" } } */
/* { dg-final { scan-assembler-not "call" } } */
