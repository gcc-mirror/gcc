/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9.4-a+sve2+fp8dot2+fp8dot4+ssve-fp8dot2+ssve-fp8dot4" } */

#include <arm_sve.h>

svfloat16_t
dot_fp8_to_f16 (svfloat16_t acc, svmfloat8_t a, svmfloat8_t b, fpm_t fpm)
{
  return svdot_f16_mf8_fpm (acc, a, b, fpm);
}

svfloat32_t
dot_fp8_to_f32 (svfloat32_t acc, svmfloat8_t a, svmfloat8_t b, fpm_t fpm)
{
  return svdot_f32_mf8_fpm (acc, a, b, fpm);
}

__arm_streaming svfloat16_t
dot_fp8_to_f16_streaming (svfloat16_t acc, svmfloat8_t a, svmfloat8_t b,
			  fpm_t fpm)
{
  return svdot_f16_mf8_fpm (acc, a, b, fpm);
}

__arm_streaming svfloat32_t
dot_fp8_to_f32_streaming (svfloat32_t acc, svmfloat8_t a, svmfloat8_t b,
			  fpm_t fpm)
{
  return svdot_f32_mf8_fpm (acc, a, b, fpm);
}

/* { dg-final { scan-assembler-times {\tfdot\tz[0-9]+\.h, z[0-9]+\.b, z[0-9]+\.b} 2 } } */
/* { dg-final { scan-assembler-times {\tfdot\tz[0-9]+\.s, z[0-9]+\.b, z[0-9]+\.b} 2 } } */
