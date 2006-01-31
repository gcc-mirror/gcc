/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -msselibm" } */
/* { dg-require-effective-target ilp32 } */

extern double acos(double);
extern double asin(double);
extern double atan(double);
extern double atan2(double, double);
extern double cos(double);
extern double exp(double);
extern double log10(double);
extern double log(double);
extern double sin(double);
extern double tan(double);

extern float acosf(float);
extern float asinf(float);
extern float atanf(float);
extern float atan2f(float, float);
extern float cosf(float);
extern float expf(float);
extern float log10f(float);
extern float logf(float);
extern float sinf(float);
extern float tanf(float);

float foof(float x)
{
  return acosf(x) + asinf(x) + atanf(x) + atan2f(x, x) + cosf(x) + expf(x)
	+ log10f(x) + logf(x) + sinf(x) + tanf(x);
}

double foo(double x)
{
  return acos(x) + asin(x) + atan(x) + atan2(x, x) + cos(x) + exp(x)
	+ log10(x) + log(x) + sin(x) + tan(x);
}

/* { dg-final { scan-assembler-times "__libm_sse2" 20 } } */
