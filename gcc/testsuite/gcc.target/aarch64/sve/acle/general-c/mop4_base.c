// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

// svmop4a[_1x1]_za32[_f32_f32]
// svmop4a[_1x1]_za32[_f16_f16]
// svmop4a[_1x1]_za32[_bf16_bf16]
// svmop4a[_1x1]_za32[_s16_s16]
// svmop4a[_1x1]_za32[_u16_u16]
// svmop4a[_1x1]_za32[_s8_s8]
// svmop4a[_1x1]_za32[_u8_u8]
// svmop4a[_1x1]_za32[_s8_u8]
// svmop4a[_1x1]_za32[_u8_s8]

#pragma GCC target "+sve2,+sme-mop4"
static_assert (__ARM_FEATURE_SME_MOP4 == 1);
#include <arm_sme.h>

void
explicit_ok (svfloat32_t f32, svfloat16_t f16, svbfloat16_t bf16, svint16_t s16,
	     svuint16_t u16, svint8_t s8,
	     svuint8_t u8) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za32_f32_f32 (0, f32, f32);
  svmop4a_1x1_za32_f16_f16 (0, f16, f16);
  svmop4a_1x1_za32_bf16_bf16 (0, bf16, bf16);
  svmop4a_1x1_za32_s16_s16 (0, s16, s16);
  svmop4a_1x1_za32_u16_u16 (0, u16, u16);
  svmop4a_1x1_za32_s8_s8 (0, s8, s8);
  svmop4a_1x1_za32_u8_u8 (0, u8, u8);
  svmop4a_1x1_za32_s8_u8 (0, s8, u8);
  svmop4a_1x1_za32_u8_s8 (0, u8, s8);
}

void
implicit_ok (svfloat32_t f32, svfloat16_t f16, svbfloat16_t bf16, svint16_t s16,
	     svuint16_t u16, svint8_t s8,
	     svuint8_t u8) __arm_streaming __arm_inout ("za")
{
  svmop4a_za32 (0, f32, f32);
  svmop4a_za32 (0, f16, f16);
  svmop4a_za32 (0, bf16, bf16);
  svmop4a_za32 (0, s16, s16);
  svmop4a_za32 (0, u16, u16);
  svmop4a_za32 (0, s8, s8);
  svmop4a_za32 (0, u8, u8);
  svmop4a_za32 (0, s8, u8);
  svmop4a_za32 (0, u8, s8);
}

void
error_not_streaming (svfloat16_t f16)
{
  svmop4a_1x1_za32_f16_f16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za32_f16_f16' can only be called when SME streaming mode is enabled} }
  svmop4a_za32 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za32_f16_f16' can only be called when SME streaming mode is enabled} }
}

void
error_streaming_compatible (svfloat16_t f16) __arm_streaming_compatible
{
  svmop4a_1x1_za32_f16_f16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za32_f16_f16' can only be called when SME streaming mode is enabled} }
  svmop4a_za32 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za32_f16_f16' can only be called when SME streaming mode is enabled} }
}

void
error_arg_count_mismatch (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za32_f16_f16 (); // { dg-error {too few arguments to function 'svmop4a_1x1_za32_f16_f16'; expected 3, have 0} }
  svmop4a_za32 (); // { dg-error {too few arguments to function 'svmop4a_za32'} }

  svmop4a_1x1_za32_f16_f16 (0, f16, f16, 0); // { dg-error {too many arguments to function 'svmop4a_1x1_za32_f16_f16'; expected 3, have 4} }
  svmop4a_za32 (0, f16, f16, 0); // { dg-error {too many arguments to function 'svmop4a_za32'} }
}

void
error_arg_type_mismatch (svfloat16_t f16, svfloat16x2_t f16x2,
			 svfloat16x4_t f16x4) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za32_f16_f16 (0, f16x2, f16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za32_f16_f16'} }
  svmop4a_za32 (0, f16x4, f16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za32_f16_f16'} }
}

void
error_zt0_not_immediate (uint64_t zt0,
			 svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za32_f16_f16 (zt0, f16, f16); // { dg-error {argument 1 of 'svmop4a_1x1_za32_f16_f16' must be an integer constant expression} }
  svmop4a_za32 (zt0, f16, f16); // { dg-error {argument 1 of 'svmop4a_za32' must be an integer constant expression} }
}

void
error_zt0_not_in_range (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za32_f16_f16 (-1, f16, f16); // { dg-error {passing -1 to argument 1 of 'svmop4a_1x1_za32_f16_f16', which expects a value in the range \[0, 3\]} }
  svmop4a_za32 (-1, f16, f16); // { dg-error {passing -1 to argument 1 of 'svmop4a_za32', which expects a value in the range \[0, 3\]} }

  svmop4a_1x1_za32_f16_f16 (4, f16, f16); // { dg-error {passing 4 to argument 1 of 'svmop4a_1x1_za32_f16_f16', which expects a value in the range \[0, 3\]} }
  svmop4a_za32 (4, f16, f16); // { dg-error {passing 4 to argument 1 of 'svmop4a_za32', which expects a value in the range \[0, 3\]} }
}

#pragma GCC target "+nothing,+sve2,+sme2"

void
error_missing_feature (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za32_f16_f16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za32_f16_f16' requires ISA extension 'sme-mop4'} }
}
