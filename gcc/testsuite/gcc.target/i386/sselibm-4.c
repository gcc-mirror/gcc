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
  return acosf(x) + asinf(x) + atanf(x) + atan2f(x, x) + cosf(x) + expf(x)
	+ log10f(x) + logf(x) + sinf(x) + tanf(x);
}

/* { dg-final { scan-assembler-times "__libm_sse2" 10 } } */
