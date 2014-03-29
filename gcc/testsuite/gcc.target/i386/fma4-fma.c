/* Test that the compiler properly optimizes floating point multiply
   and add instructions into vfmaddss, vfmsubss, vfnmaddss,
   vfnmsubss on FMA4 systems.  */

/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mfma4 -mno-fma" } */

extern void exit (int);

float
flt_mul_add (float a, float b, float c)
{
  return (a * b) + c;
}

double
dbl_mul_add (double a, double b, double c)
{
  return (a * b) + c;
}

float
flt_mul_sub (float a, float b, float c)
{
  return (a * b) - c;
}

double
dbl_mul_sub (double a, double b, double c)
{
  return (a * b) - c;
}

float
flt_neg_mul_add (float a, float b, float c)
{
  return (-(a * b)) + c;
}

double
dbl_neg_mul_add (double a, double b, double c)
{
  return (-(a * b)) + c;
}

float
flt_neg_mul_sub (float a, float b, float c)
{
  return (-(a * b)) - c;
}

double
dbl_neg_mul_sub (double a, double b, double c)
{
  return (-(a * b)) - c;
}

float  f[10] = { 2, 3, 4 };
double d[10] = { 2, 3, 4 };

int main ()
{
  f[3] = flt_mul_add (f[0], f[1], f[2]);
  f[4] = flt_mul_sub (f[0], f[1], f[2]);
  f[5] = flt_neg_mul_add (f[0], f[1], f[2]);
  f[6] = flt_neg_mul_sub (f[0], f[1], f[2]);

  d[3] = dbl_mul_add (d[0], d[1], d[2]);
  d[4] = dbl_mul_sub (d[0], d[1], d[2]);
  d[5] = dbl_neg_mul_add (d[0], d[1], d[2]);
  d[6] = dbl_neg_mul_sub (d[0], d[1], d[2]);
  exit (0);
}

/* { dg-final { scan-assembler "vfmaddss" } } */
/* { dg-final { scan-assembler "vfmaddsd" } } */
/* { dg-final { scan-assembler "vfmsubss" } } */
/* { dg-final { scan-assembler "vfmsubsd" } } */
/* { dg-final { scan-assembler "vfnmaddss" } } */
/* { dg-final { scan-assembler "vfnmaddsd" } } */
/* { dg-final { scan-assembler "vfnmsubss" } } */
/* { dg-final { scan-assembler "vfnmsubsd" } } */
