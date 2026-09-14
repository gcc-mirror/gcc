/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme-tmop")

void
f1 (uint64_t u64,
    svfloat32x2_t f32x2, svfloat32_t f32,
    svfloat16x2_t f16x2, svfloat16_t f16,
    svint8x2_t s8x2, svint8_t s8,
    svuint8x2_t u8x2, svuint8_t u8,
    svint16_t s16, svuint16_t u16)
  __arm_streaming __arm_inout("za")
{
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, 0);

  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8); /* { dg-error {too few arguments to function 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, 0, 0); /* { dg-error {too many arguments to function 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (u64, f32x2, f32, u8, 0); /* { dg-error {argument 1 of 'svtmopa_lane_za32_f32_f32' must be an integer constant expression} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, u64); /* { dg-error {argument 5 of 'svtmopa_lane_za32_f32_f32' must be an integer constant expression} } */

  svtmopa_lane_za32_f32_f32 (-1, f32x2, f32, u8, 0); /* { dg-error {passing -1 to argument 1 of 'svtmopa_lane_za32_f32_f32', which expects a value in the range \[0, 3\]} } */
  svtmopa_lane_za32_f32_f32 (4, f32x2, f32, u8, 0); /* { dg-error {passing 4 to argument 1 of 'svtmopa_lane_za32_f32_f32', which expects a value in the range \[0, 3\]} } */

  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, -1); /* { dg-error {passing -1 to argument 5 of 'svtmopa_lane_za32_f32_f32', which expects a value in the range \[0, 3\]} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, 4); /* { dg-error {passing 4 to argument 5 of 'svtmopa_lane_za32_f32_f32', which expects a value in the range \[0, 3\]} } */

  svtmopa_lane_za32_f32_f32 (0, u8, f32, u8, 0); /* { dg-error {incompatible type for argument 2 of 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f32, f32, u8, 0); /* { dg-error {incompatible type for argument 2 of 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f16x2, f32, u8, 0); /* { dg-error {incompatible type for argument 2 of 'svtmopa_lane_za32_f32_f32'} } */

  svtmopa_lane_za32_f32_f32 (0, f32x2, f16, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32x2, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, u8, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svtmopa_lane_za32_f32_f32'} } */

  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u16, 0); /* { dg-error {incompatible type for argument 4 of 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, 0, 0); /* { dg-error {incompatible type for argument 4 of 'svtmopa_lane_za32_f32_f32'} } */
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, f32, 0); /* { dg-error {incompatible type for argument 4 of 'svtmopa_lane_za32_f32_f32'} } */

  svtmopa_lane_za32_s8_u8(0, s8x2, u8, u8, 0);
  svtmopa_lane_za32_s8_u8(0, u8x2, u8, u8, 0); /* { dg-error {incompatible type for argument 2 of 'svtmopa_lane_za32_s8_u8'} } */
  svtmopa_lane_za32_s8_u8(0, s8x2, s8, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svtmopa_lane_za32_s8_u8'} } */
  svtmopa_lane_za32_u8_s8(0, s8x2, s8, u8, 0); /* { dg-error {incompatible type for argument 2 of 'svtmopa_lane_za32_u8_s8'} } */
  svtmopa_lane_za32_u8_s8(0, u8x2, u8, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svtmopa_lane_za32_u8_s8'} } */
}

void
f2 (svfloat32x2_t f32x2, svfloat32_t f32, svuint8_t u8) __arm_streaming
{
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, 0); /* { dg-error {ACLE function 'svtmopa_lane_za32_f32_f32' can only be called from a function that has 'za' state} } */
}

void
f3 (svfloat32x2_t f32x2, svfloat32_t f32, svuint8_t u8) __arm_inout("za")
{
  svtmopa_lane_za32_f32_f32 (0, f32x2, f32, u8, 0); /* { dg-error {ACLE function 'svtmopa_lane_za32_f32_f32' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("arch=armv9-a+sme-tmop+sme-f8f16")

void
f4 (svmfloat8x2_t mf8x2, svmfloat8_t mf8, svuint8_t u8, fpm_t fpm)
  __arm_streaming __arm_inout("za")
{

  svtmopa_lane_za16_mf8_mf8_fpm (0, mf8x2, mf8, u8); /* { dg-error {too few arguments to function 'svtmopa_lane_za16_mf8_mf8_fpm'} } */
  svtmopa_lane_za16_mf8_mf8_fpm (0, mf8x2, mf8, u8, 0, 0, fpm); /* { dg-error {too many arguments to function 'svtmopa_lane_za16_mf8_mf8_fpm'} } */
  svtmopa_lane_za16_mf8_mf8_fpm (-1, mf8x2, mf8, u8, 0, fpm); /* { dg-error {passing -1 to argument 1 of 'svtmopa_lane_za16_mf8_mf8_fpm', which expects a value in the range \[0, 1\]} } */
  svtmopa_lane_za16_mf8_mf8_fpm (2,  mf8x2, mf8, u8, 0, fpm); /* { dg-error {passing 2 to argument 1 of 'svtmopa_lane_za16_mf8_mf8_fpm', which expects a value in the range \[0, 1\]} } */
  svtmopa_lane_za16_mf8_mf8_fpm (0, mf8x2, mf8, u8, 0, mf8); /* { dg-error {incompatible type for argument 6 of 'svtmopa_lane_za16_mf8_mf8_fpm'} } */
}

#pragma GCC target ("arch=armv9-a+sme-tmop+sme-f16f16")

void
f5 (svfloat16x2_t f16x2, svfloat16_t f16,
    svuint8_t u8)
  __arm_streaming __arm_inout("za")
{
  svtmopa_lane_za16_f16_f16 (-1, f16x2, f16, u8, 0); /* { dg-error {passing -1 to argument 1 of 'svtmopa_lane_za16_f16_f16', which expects a value in the range \[0, 1\]} } */
  svtmopa_lane_za16_f16_f16 (2, f16x2, f16, u8, 0); /* { dg-error {passing 2 to argument 1 of 'svtmopa_lane_za16_f16_f16', which expects a value in the range \[0, 1\]} } */

  svtmopa_lane_za16_f16_f16 (1, f16x2, f16, u8, -1); /* { dg-error {passing -1 to argument 5 of 'svtmopa_lane_za16_f16_f16', which expects a value in the range \[0, 3\]} } */
  svtmopa_lane_za16_f16_f16 (1, f16x2, f16, u8, 4); /* { dg-error {passing 4 to argument 5 of 'svtmopa_lane_za16_f16_f16', which expects a value in the range \[0, 3\]} } */
}

