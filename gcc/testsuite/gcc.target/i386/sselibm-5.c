/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -msselibm" } */
/* { dg-require-effective-target ilp32 } */

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

/* { dg-final { scan-assembler-times "__libm_sse2" 10 } } */
