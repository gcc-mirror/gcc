/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv9.2-a+sve2+lut")

void
test (svfloat16_t f16, svfloat32_t f32, svfloat64_t f64,
      svfloat32x2_t f32x2, svfloat64x2_t f64x2,
      svfloat16x3_t f16x3,
      svuint8_t u8, svuint16_t u16, svuint32_t u32, svuint64_t u64,
      svuint8x2_t u8x2, svuint32x2_t u32x2, svuint64x2_t u64x2,
      svuint16x3_t u16x3,
      svint8_t s8, svint16_t s16, svint32_t s32, svint64_t s64,
      svint8x2_t s8x2, svint32x2_t s32x2, svint64x2_t s64x2,
      svint16x3_t s16x3,
      svbfloat16_t bf16, svbfloat16x2_t bf16x2,
      svbfloat16x3_t bf16x3,
      int idx)
{
  svluti2_lane_f16 (f16); /* { dg-error {too few arguments to function 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f16, u8); /* { dg-error {too few arguments to function 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f16, u8, 0, f16); /* { dg-error {too many arguments to function 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f16, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti2_lane_f16'} } */
  svluti2_lane_f16 (f16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane_f16' must be an integer constant expression} } */

  svluti2_lane (f16); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (f16, u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (f16, u8, 0, f16); /* { dg-error {too many arguments to function 'svluti2_lane'} } */
  svluti2_lane (f16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (f16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (f16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti2_lane', which expects 'uint64_t'} } */
  svluti2_lane (f16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane' must be an integer constant expression} } */
  svluti2_lane (f32, u8, 0); /* { dg-error {'svluti2_lane' has no form that takes 'svfloat32_t' arguments} } */
  svluti2_lane (f64, u8, 0); /* { dg-error {'svluti2_lane' has no form that takes 'svfloat64_t' arguments} } */

  svluti2_lane_bf16 (bf16); /* { dg-error {too few arguments to function 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (bf16, u8); /* { dg-error {too few arguments to function 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (bf16, u8, 0, bf16); /* { dg-error {too many arguments to function 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (f32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (bf16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (bf16, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (bf16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti2_lane_bf16'} } */
  svluti2_lane_bf16 (bf16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane_bf16' must be an integer constant expression} } */

  svluti2_lane (bf16); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (bf16, u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (bf16, u8, 0, bf16); /* { dg-error {too many arguments to function 'svluti2_lane'} } */
  svluti2_lane (bf16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (bf16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (bf16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti2_lane', which expects 'uint64_t'} } */
  svluti2_lane (bf16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane' must be an integer constant expression} } */

  svluti2_lane_u8 (u8); /* { dg-error {too few arguments to function 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u8, u8); /* { dg-error {too few arguments to function 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u8, u8, 0, u8); /* { dg-error {too many arguments to function 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u8, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u8, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u8, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti2_lane_u8'} } */
  svluti2_lane_u8 (u8, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane_u8' must be an integer constant expression} } */

  svluti2_lane (u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (u8, u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (u8, u8, 0, u8); /* { dg-error {too many arguments to function 'svluti2_lane'} } */
  svluti2_lane (u8, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (u8, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti2_lane', which expects 'uint64_t'} } */
  svluti2_lane (u8, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane' must be an integer constant expression} } */

  svluti2_lane_u16 (u16); /* { dg-error {too few arguments to function 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u16, u8); /* { dg-error {too few arguments to function 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u16, u8, 0, u16); /* { dg-error {too many arguments to function 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u16, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti2_lane_u16'} } */
  svluti2_lane_u16 (u16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane_u16' must be an integer constant expression} } */

  svluti2_lane (u16); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (u16, u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (u16, u8, 0, u16); /* { dg-error {too many arguments to function 'svluti2_lane'} } */
  svluti2_lane (u16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (u16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (u16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti2_lane', which expects 'uint64_t'} } */
  svluti2_lane (u16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane' must be an integer constant expression} } */
  svluti2_lane (u32, u8, 0); /* { dg-error {'svluti2_lane' has no form that takes 'svuint32_t' arguments} } */
  svluti2_lane (u64, u8, 0); /* { dg-error {'svluti2_lane' has no form that takes 'svuint64_t' arguments} } */

  svluti2_lane_s8 (s8); /* { dg-error {too few arguments to function 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s8, u8); /* { dg-error {too few arguments to function 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s8, u8, 0, s8); /* { dg-error {too many arguments to function 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s8, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s8, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s8, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti2_lane_s8'} } */
  svluti2_lane_s8 (s8, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane_s8' must be an integer constant expression} } */
  
  svluti2_lane (s8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (s8, u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (s8, u8, 0, s8); /* { dg-error {too many arguments to function 'svluti2_lane'} } */
  svluti2_lane (s8, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (s8, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (s8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti2_lane', which expects 'uint64_t'} } */
  svluti2_lane (s8, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane' must be an integer constant expression} } */
  svluti2_lane (s32, u8, 0); /* { dg-error {'svluti2_lane' has no form that takes 'svint32_t' arguments} } */
  svluti2_lane (s64, u8, 0); /* { dg-error {'svluti2_lane' has no form that takes 'svint64_t' arguments} } */

  svluti2_lane_s16 (s16); /* { dg-error {too few arguments to function 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s16, u8); /* { dg-error {too few arguments to function 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s16, u8, 0, s16); /* { dg-error {too many arguments to function 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s16, s16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti2_lane_s16'} } */
  svluti2_lane_s16 (s16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane_s16' must be an integer constant expression} } */

  svluti2_lane (s16); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (s16, u8); /* { dg-error {too few arguments to function 'svluti2_lane'} } */
  svluti2_lane (s16, u8, 0, s16); /* { dg-error {too many arguments to function 'svluti2_lane'} } */
  svluti2_lane (s16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (s16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti2_lane', which expects 'svuint8_t'} } */
  svluti2_lane (s16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti2_lane', which expects 'uint64_t'} } */
  svluti2_lane (s16, u8, idx); /* { dg-error {argument 3 of 'svluti2_lane' must be an integer constant expression} } */

  svluti4_lane_f16 (f16); /* { dg-error {too few arguments to function 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f16, u8); /* { dg-error {too few arguments to function 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f16, u8, 0, f16); /* { dg-error {too many arguments to function 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f16, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti4_lane_f16'} } */
  svluti4_lane_f16 (f16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane_f16' must be an integer constant expression} } */

  svluti4_lane (f16); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (f16, u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (f16, u8, 0, f16); /* { dg-error {too many arguments to function 'svluti4_lane'} } */
  svluti4_lane (f16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (f16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (f16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti4_lane', which expects 'uint64_t'} } */
  svluti4_lane (f16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane' must be an integer constant expression} } */
  svluti4_lane (f32, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svfloat32_t' arguments} } */
  svluti4_lane (f64, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svfloat64_t' arguments} } */
  svluti4_lane (f16x3, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svfloat16x3_t' arguments} } */
  svluti4_lane (f32x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svfloat32x2_t' arguments} } */
  svluti4_lane (f64x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svfloat64x2_t' arguments} } */

  svluti4_lane_bf16 (bf16); /* { dg-error {too few arguments to function 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (bf16, u8); /* { dg-error {too few arguments to function 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (bf16, u8, 0, bf16); /* { dg-error {too many arguments to function 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (f32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (bf16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (bf16, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (bf16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti4_lane_bf16'} } */
  svluti4_lane_bf16 (bf16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane_bf16' must be an integer constant expression} } */

  svluti4_lane (bf16); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (bf16, u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (bf16, u8, 0, bf16); /* { dg-error {too many arguments to function 'svluti4_lane'} } */
  svluti4_lane (bf16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (bf16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (bf16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti4_lane', which expects 'uint64_t'} } */
  svluti4_lane (bf16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane' must be an integer constant expression} } */

  svluti4_lane_u8 (u8); /* { dg-error {too few arguments to function 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u8, u8); /* { dg-error {too few arguments to function 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u8, u8, 0, u8); /* { dg-error {too many arguments to function 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u8, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u8, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u8, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti4_lane_u8'} } */
  svluti4_lane_u8 (u8, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane_u8' must be an integer constant expression} } */

  svluti4_lane (u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (u8, u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (u8, u8, 0, u8); /* { dg-error {too many arguments to function 'svluti4_lane'} } */
  svluti4_lane (u8, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (u8, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti4_lane', which expects 'uint64_t'} } */
  svluti4_lane (u8, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane' must be an integer constant expression} } */

  svluti4_lane_u16 (u16); /* { dg-error {too few arguments to function 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u16, u8); /* { dg-error {too few arguments to function 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u16, u8, 0, u16); /* { dg-error {too many arguments to function 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u16, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti4_lane_u16'} } */
  svluti4_lane_u16 (u16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane_u16' must be an integer constant expression} } */

  svluti4_lane (u16); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (u16, u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (u16, u8, 0, u16); /* { dg-error {too many arguments to function 'svluti4_lane'} } */
  svluti4_lane (u16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (u16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (u16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti4_lane', which expects 'uint64_t'} } */
  svluti4_lane (u16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane' must be an integer constant expression} } */
  svluti4_lane (u32, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svuint32_t' arguments} } */
  svluti4_lane (u64, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svuint64_t' arguments} } */
  svluti4_lane (u8x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svuint8x2_t' arguments} } */
  svluti4_lane (u16x3, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svuint16x3_t' arguments} } */
  svluti4_lane (u32x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svuint32x2_t' arguments} } */
  svluti4_lane (u64x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svuint64x2_t' arguments} } */

  svluti4_lane_s8 (s8); /* { dg-error {too few arguments to function 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s8, u8); /* { dg-error {too few arguments to function 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s8, u8, 0, s8); /* { dg-error {too many arguments to function 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s8, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s8, u16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s8, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti4_lane_s8'} } */
  svluti4_lane_s8 (s8, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane_s8' must be an integer constant expression} } */
  
  svluti4_lane (s8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (s8, u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (s8, u8, 0, s8); /* { dg-error {too many arguments to function 'svluti4_lane'} } */
  svluti4_lane (s8, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (s8, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (s8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti4_lane', which expects 'uint64_t'} } */
  svluti4_lane (s8, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane' must be an integer constant expression} } */

  svluti4_lane_s16 (s16); /* { dg-error {too few arguments to function 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s16, u8); /* { dg-error {too few arguments to function 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s16, u8, 0, s16); /* { dg-error {too many arguments to function 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (0, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s32, u8, 0); /* { dg-error {incompatible type for argument 1 of 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s16, 0, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s16, s16, 0); /* { dg-error {incompatible type for argument 2 of 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s16, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svluti4_lane_s16'} } */
  svluti4_lane_s16 (s16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane_s16' must be an integer constant expression} } */

  svluti4_lane (s16); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (s16, u8); /* { dg-error {too few arguments to function 'svluti4_lane'} } */
  svluti4_lane (s16, u8, 0, s16); /* { dg-error {too many arguments to function 'svluti4_lane'} } */
  svluti4_lane (s16, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (s16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svluti4_lane', which expects 'svuint8_t'} } */
  svluti4_lane (s16, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svluti4_lane', which expects 'uint64_t'} } */
  svluti4_lane (s16, u8, idx); /* { dg-error {argument 3 of 'svluti4_lane' must be an integer constant expression} } */
  svluti4_lane (s32, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svint32_t' arguments} } */
  svluti4_lane (s64, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svint64_t' arguments} } */
  svluti4_lane (s8x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svint8x2_t' arguments} } */
  svluti4_lane (s16x3, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svint16x3_t' arguments} } */
  svluti4_lane (s32x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svint32x2_t' arguments} } */
  svluti4_lane (s64x2, u8, 0); /* { dg-error {'svluti4_lane' has no form that takes 'svint64x2_t' arguments} } */

  svluti2_lane (0, u8, 0); /* { dg-error {passing 'int' to argument 1 of 'svluti2_lane', which expects an SVE type rather than a scalar type} } */
  svluti4_lane (0, u8, 0); /* { dg-error {passing 'int' to argument 1 of 'svluti4_lane', which expects an SVE type rather than a scalar type} } */
}
