/* Test that the compiler properly optimizes floating point multiply
   and add instructions into vfmaddss, vfmsubss, vfnmaddss,
   vfnmsubss on FMA4 systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -funsafe-math-optimizations -mfma4" } */

extern void exit (int);

float
flt_mul_add (float a, float c)
{
  return (a * a) + c;
}

double
dbl_mul_add (double a, double c)
{
  return (a * a) + c;
}

float
flt_mul_sub (float a, float c)
{
  return (a * a) - c;
}

double
dbl_mul_sub (double a, double c)
{
  return (a * a) - c;
}

float
flt_neg_mul_add (float a, float c)
{
  return (-(a * a)) + c;
}

double
dbl_neg_mul_add (double a, double c)
{
  return (-(a * a)) + c;
}

float  f[10] = { 2, 3, 4 };
double d[10] = { 2, 3, 4 };

int main ()
{
  f[3] = flt_mul_add (f[0], f[2]);
  f[4] = flt_mul_sub (f[0], f[2]);
  f[5] = flt_neg_mul_add (f[0], f[2]);

  d[3] = dbl_mul_add (d[0], d[2]);
  d[4] = dbl_mul_sub (d[0], d[2]);
  d[5] = dbl_neg_mul_add (d[0], d[2]);
  exit (0);
}

/* { dg-final { scan-assembler "vfmaddss" } } */
/* { dg-final { scan-assembler "vfmaddsd" } } */
/* { dg-final { scan-assembler "vfmsubss" } } */
/* { dg-final { scan-assembler "vfmsubsd" } } */
/* { dg-final { scan-assembler "vfnmaddss" } } */
/* { dg-final { scan-assembler "vfnmaddsd" } } */
