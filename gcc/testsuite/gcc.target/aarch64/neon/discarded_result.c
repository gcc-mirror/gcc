/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-additional-options "-ftrapping-math" } */

#include "arm_neon_test.h"

/* Assert that discarding the result of a NEON intrinsic does not cause a crash during folding.  */

/*
** foo:
** fadd	v[0-9]+\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
void foo(float32x2_t a, float32x2_t b)
{
  vadd_f32 (a, b);
}

/*
** test_vmull_s8:
** ret
*/
void test_vmull_s8 (int8x8_t a, int8x8_t b)
{
  vmull_s8 (a, b);
}

/*
** test_vmull_high_s16:
** ret
*/
void test_vmull_high_s16 (int16x8_t a, int16x8_t b)
{
  vmull_high_s16 (a, b);
}

/*
** test_vmull_s32:
** ret
*/
void test_vmull_s32 (int32x2_t a, int32x2_t b)
{
  vmull_s32 (a, b);
}

/*
** test_vmlal_u8:
** ret
*/
void test_vmlal_u8 (uint16x8_t acc, uint8x8_t a, uint8x8_t b)
{
  vmlal_u8 (acc, a, b);
}

/*
** test_vmlsl_high_s16:
** ret
*/
void test_vmlsl_high_s16 (int32x4_t acc, int16x8_t a, int16x8_t b)
{
  vmlsl_high_s16 (acc, a, b);
}

/*
** test_vmull_high_redirect:
** ret
*/
void test_vmull_high_redirect (int32x4_t x, int32x4_t y)
{
  vmull_s32 (vget_high_s32 (x), vget_high_s32 (y));
}

/*
** test_vmull_splat_redirect:
** ret
*/
void test_vmull_splat_redirect (int8x16_t x)
{
  vmull_s8 (vget_high_s8 (x), vdup_n_s8 (3));
}
