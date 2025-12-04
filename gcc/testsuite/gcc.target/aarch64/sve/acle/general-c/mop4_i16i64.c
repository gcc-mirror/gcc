// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

// svmop4a[_1x1]_za64[_s16_s16] (only if __ARM_FEATURE_SME_I16I64 != 0)
// svmop4a[_1x1]_za64[_u16_u16] (only if __ARM_FEATURE_SME_I16I64 != 0)
// svmop4a[_1x1]_za64[_s16_u16] (only if __ARM_FEATURE_SME_I16I64 != 0)
// svmop4a[_1x1]_za64[_u16_s16] (only if __ARM_FEATURE_SME_I16I64 != 0)

#pragma GCC target "+sve2,+sme-mop4,+sme-i16i64"
static_assert (__ARM_FEATURE_SME_MOP4 == 1);
static_assert (__ARM_FEATURE_SME_I16I64 == 1);
#include <arm_sme.h>

void
explicit_ok (svint16_t s16, svuint16_t u16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_s16_s16 (0, s16, s16);
  svmop4a_1x1_za64_u16_u16 (0, u16, u16);
  svmop4a_1x1_za64_s16_u16 (0, s16, u16);
  svmop4a_1x1_za64_u16_s16 (0, u16, s16);
}

void
implicit_ok (svint16_t s16, svuint16_t u16) __arm_streaming __arm_inout ("za")
{
  svmop4a_za64 (0, s16, s16);
  svmop4a_za64 (0, u16, u16);
  svmop4a_za64 (0, s16, u16);
  svmop4a_za64 (0, u16, s16);
}

void
error_not_streaming (svint16_t s16)
{
  svmop4a_1x1_za64_s16_s16 (0, s16, s16); // { dg-error {ACLE function 'svmop4a_1x1_za64_s16_s16' can only be called when SME streaming mode is enabled} }
  svmop4a_za64 (0, s16, s16); // { dg-error {ACLE function 'svmop4a_1x1_za64_s16_s16' can only be called when SME streaming mode is enabled} }
}

void
error_streaming_compatible (svint16_t s16) __arm_streaming_compatible
{
  svmop4a_1x1_za64_s16_s16 (0, s16, s16); // { dg-error {ACLE function 'svmop4a_1x1_za64_s16_s16' can only be called when SME streaming mode is enabled} }
  svmop4a_za64 (0, s16, s16); // { dg-error {ACLE function 'svmop4a_1x1_za64_s16_s16' can only be called when SME streaming mode is enabled} }
}

void
error_arg_count_mismatch (svint16_t s16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_s16_s16 (); // { dg-error {too few arguments to function 'svmop4a_1x1_za64_s16_s16'; expected 3, have 0} }
  svmop4a_za64 (); // { dg-error {too few arguments to function 'svmop4a_za64'} }

  svmop4a_1x1_za64_s16_s16 (0, s16, s16, 0); // { dg-error {too many arguments to function 'svmop4a_1x1_za64_s16_s16'; expected 3, have 4} }
  svmop4a_za64 (0, s16, s16, 0); // { dg-error {too many arguments to function 'svmop4a_za64'} }
}

void
error_arg_type_mismatch (svint16_t s16, svint16x2_t s16x2,
			 svint16x4_t s16x4) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_s16_s16 (0, s16x2, s16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za64_s16_s16'} }
  svmop4a_za64 (0, s16x4, s16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za64_s16_s16'} }
}

void
error_zt0_not_immediate (uint64_t zt0,
			 svint16_t s16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_s16_s16 (zt0, s16, s16); // { dg-error {argument 1 of 'svmop4a_1x1_za64_s16_s16' must be an integer constant expression} }
  svmop4a_za64 (zt0, s16, s16); // { dg-error {argument 1 of 'svmop4a_za64' must be an integer constant expression} }
}

void
error_zt0_not_in_range (svint16_t s16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_s16_s16 (-1, s16, s16); // { dg-error {passing -1 to argument 1 of 'svmop4a_1x1_za64_s16_s16', which expects a value in the range \[0, 7\]} }
  svmop4a_za64 (-1, s16, s16); // { dg-error {passing -1 to argument 1 of 'svmop4a_za64', which expects a value in the range \[0, 7\]} }

  svmop4a_1x1_za64_s16_s16 (8, s16, s16); // { dg-error {passing 8 to argument 1 of 'svmop4a_1x1_za64_s16_s16', which expects a value in the range \[0, 7\]} }
  svmop4a_za64 (8, s16, s16); // { dg-error {passing 8 to argument 1 of 'svmop4a_za64', which expects a value in the range \[0, 7\]} }
}

#pragma GCC target "+nothing,+sve2,+sme2,+sme-mop4"

void
error_missing_feature (svint16_t s16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_s16_s16 (0, s16, s16); // { dg-error {ACLE function 'svmop4a_1x1_za64_s16_s16' requires ISA extension 'sme-i16i64'} }
}
