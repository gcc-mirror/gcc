/* Test whether using target specific options, we can generate SSE5 code.  */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -march=k8" } */

extern void exit (int);

#define SSE5_ATTR __attribute__((__target__("sse5,fused-madd")))
extern float  flt_mul_add     (float a, float b, float c) SSE5_ATTR;
extern float  flt_mul_sub     (float a, float b, float c) SSE5_ATTR;
extern float  flt_neg_mul_add (float a, float b, float c) SSE5_ATTR;
extern float  flt_neg_mul_sub (float a, float b, float c) SSE5_ATTR;

extern double dbl_mul_add     (double a, double b, double c) SSE5_ATTR;
extern double dbl_mul_sub     (double a, double b, double c) SSE5_ATTR;
extern double dbl_neg_mul_add (double a, double b, double c) SSE5_ATTR;
extern double dbl_neg_mul_sub (double a, double b, double c) SSE5_ATTR;

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

/* { dg-final { scan-assembler "fmaddss" } } */
/* { dg-final { scan-assembler "fmaddsd" } } */
/* { dg-final { scan-assembler "fmsubss" } } */
/* { dg-final { scan-assembler "fmsubsd" } } */
/* { dg-final { scan-assembler "fnmaddss" } } */
/* { dg-final { scan-assembler "fnmaddsd" } } */
/* { dg-final { scan-assembler "fnmsubss" } } */
/* { dg-final { scan-assembler "fnmsubsd" } } */
/* { dg-final { scan-assembler "call\t(.*)flt_mul_add" } } */
/* { dg-final { scan-assembler "call\t(.*)flt_mul_sub" } } */
/* { dg-final { scan-assembler "call\t(.*)flt_neg_mul_add" } } */
/* { dg-final { scan-assembler "call\t(.*)flt_neg_mul_sub" } } */
/* { dg-final { scan-assembler "call\t(.*)dbl_mul_add" } } */
/* { dg-final { scan-assembler "call\t(.*)dbl_mul_sub" } } */
/* { dg-final { scan-assembler "call\t(.*)dbl_neg_mul_add" } } */
/* { dg-final { scan-assembler "call\t(.*)dbl_neg_mul_sub" } } */
