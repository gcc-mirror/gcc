// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

// svmop4a[_1x1]_za16[_f16_f16] (only if __ARM_FEATURE_SME_F16F16 != 0)

#pragma GCC target "+sve2,+sme-mop4,+sme-f16f16"
static_assert (__ARM_FEATURE_SME_MOP4 == 1);
static_assert (__ARM_FEATURE_SME_F16F16 == 1);
#include <arm_sme.h>

void
explicit_ok (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_f16_f16 (0, f16, f16);
}

void
implicit_ok (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_za16 (0, f16, f16);
}

void
error_not_streaming (svfloat16_t f16)
{
  svmop4a_1x1_za16_f16_f16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za16_f16_f16' can only be called when SME streaming mode is enabled} }
  svmop4a_za16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za16_f16_f16' can only be called when SME streaming mode is enabled} }
}

void
error_streaming_compatible (svfloat16_t f16) __arm_streaming_compatible
{
  svmop4a_1x1_za16_f16_f16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za16_f16_f16' can only be called when SME streaming mode is enabled} }
  svmop4a_za16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za16_f16_f16' can only be called when SME streaming mode is enabled} }
}

void
error_arg_count_mismatch (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_f16_f16 (); // { dg-error {too few arguments to function 'svmop4a_1x1_za16_f16_f16'; expected 3, have 0} }
  svmop4a_za16 (); // { dg-error {too few arguments to function 'svmop4a_za16'} }

  svmop4a_1x1_za16_f16_f16 (0, f16, f16, 0); // { dg-error {too many arguments to function 'svmop4a_1x1_za16_f16_f16'; expected 3, have 4} }
  svmop4a_za16 (0, f16, f16, 0); // { dg-error {too many arguments to function 'svmop4a_za16'} }
}

void
error_arg_type_mismatch (svfloat16_t f16, svfloat16x2_t f16x2,
			 svfloat16x4_t f16x4) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_f16_f16 (0, f16x2, f16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za16_f16_f16'} }
  svmop4a_za16 (0, f16x4, f16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za16_f16_f16'} }
}

void
error_zt0_not_immediate (uint64_t zt0,
			 svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_f16_f16 (zt0, f16, f16); // { dg-error {argument 1 of 'svmop4a_1x1_za16_f16_f16' must be an integer constant expression} }
  svmop4a_za16 (zt0, f16, f16); // { dg-error {argument 1 of 'svmop4a_za16' must be an integer constant expression} }
}

void
error_zt0_not_in_range (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_f16_f16 (-1, f16, f16); // { dg-error {passing -1 to argument 1 of 'svmop4a_1x1_za16_f16_f16', which expects a value in the range \[0, 1\]} }
  svmop4a_za16 (-1, f16, f16); // { dg-error {passing -1 to argument 1 of 'svmop4a_za16', which expects a value in the range \[0, 1\]} }

  svmop4a_1x1_za16_f16_f16 (2, f16, f16); // { dg-error {passing 2 to argument 1 of 'svmop4a_1x1_za16_f16_f16', which expects a value in the range \[0, 1\]} }
  svmop4a_za16 (2, f16, f16); // { dg-error {passing 2 to argument 1 of 'svmop4a_za16', which expects a value in the range \[0, 1\]} }
}

#pragma GCC target "+nothing,+sve2,+sme2,+sme-mop4"

void
error_missing_feature (svfloat16_t f16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_f16_f16 (0, f16, f16); // { dg-error {ACLE function 'svmop4a_1x1_za16_f16_f16' requires ISA extension 'sme-f16f16'} }
}
