/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2 -std=c11" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

/* Test compiler use of FP16 instructions.  */
#include <arm_fp16.h>

float16_t
test_mov_imm_1 (float16_t a)
{
  return 1.0;
}

float16_t
test_mov_imm_2 (float16_t a)
{
  float16_t b = 1.0;
  return b;
}

float16_t
test_vmov_imm_3 (float16_t a)
{
  float16_t b = 1.0;
  return vaddh_f16 (a, b);
}

float16_t
test_vmov_imm_4 (float16_t a)
{
  return vaddh_f16 (a, 1.0);
}

/* { dg-final { scan-assembler-times {vmov.f16\ts[0-9]+, #1\.0e\+0} 4 } }
   { dg-final { scan-assembler-times {vadd.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 2 } } */

float16_t
test_vmla_1 (float16_t a, float16_t b, float16_t c)
{
  return vaddh_f16 (vmulh_f16 (a, b), c);
}
/* { dg-final { scan-assembler-times {vmla\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

float16_t
test_vmla_2 (float16_t a, float16_t b, float16_t c)
{
  return vsubh_f16 (vmulh_f16 (vnegh_f16 (a), b), c);
}
/* { dg-final { scan-assembler-times {vnmla\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } } */

float16_t
test_vmls_1 (float16_t a, float16_t b, float16_t c)
{
  return vsubh_f16 (c, vmulh_f16 (a, b));
}

float16_t
test_vmls_2 (float16_t a, float16_t b, float16_t c)
{
  return vsubh_f16 (a, vmulh_f16 (b, c));
}
/* { dg-final { scan-assembler-times {vmls\.f16} 2 } } */

float16_t
test_vnmls_1 (float16_t a, float16_t b, float16_t c)
{
  return vsubh_f16 (vmulh_f16 (a, b), c);
}
/* { dg-final { scan-assembler-times {vnmls\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } } */

