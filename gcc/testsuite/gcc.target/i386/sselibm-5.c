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
  return __builtin_sse2_acosf(x)
	+ __builtin_sse2_asinf(x)
	+ __builtin_sse2_atanf(x)
	+ __builtin_sse2_atan2f(x, x)
	+ __builtin_sse2_cosf(x)
	+ __builtin_sse2_expf(x)
	+ __builtin_sse2_log10f(x)
	+ __builtin_sse2_logf(x)
	+ __builtin_sse2_sinf(x)
	+ __builtin_sse2_tanf(x);
}

double foo(double x)
{
  return __builtin_sse2_acos(x)
	+ __builtin_sse2_asin(x)
	+ __builtin_sse2_atan(x)
	+ __builtin_sse2_atan2(x, x)
	+ __builtin_sse2_cos(x)
	+ __builtin_sse2_exp(x)
	+ __builtin_sse2_log10(x)
	+ __builtin_sse2_log(x)
	+ __builtin_sse2_sin(x)
	+ __builtin_sse2_tan(x);
}

/* { dg-final { scan-assembler-times "__libm_sse2" 20 } } */
