// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

// svmop4a[_1x1]_za16[_bbf16_bbf16] (only if __ARM_FEATURE_SME_B16B16 != 0)

#pragma GCC target "+sve2,+sme-mop4,+sme-b16b16"
static_assert (__ARM_FEATURE_SME_MOP4 == 1);
static_assert (__ARM_FEATURE_SME_B16B16 == 1);
#include <arm_sme.h>

void
explicit_ok (svbfloat16_t bf16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_bf16_bf16 (0, bf16, bf16);
}

void
implicit_ok (svbfloat16_t bf16) __arm_streaming __arm_inout ("za")
{
  svmop4a_za16 (0, bf16, bf16);
}

void
error_not_streaming (svbfloat16_t bf16)
{
  svmop4a_1x1_za16_bf16_bf16 (0, bf16, bf16); // { dg-error {ACLE function 'svmop4a_1x1_za16_bf16_bf16' can only be called when SME streaming mode is enabled} }
  svmop4a_za16 (0, bf16, bf16); // { dg-error {ACLE function 'svmop4a_1x1_za16_bf16_bf16' can only be called when SME streaming mode is enabled} }
}

void
error_streaming_compatible (svbfloat16_t bf16) __arm_streaming_compatible
{
  svmop4a_1x1_za16_bf16_bf16 (0, bf16, bf16); // { dg-error {ACLE function 'svmop4a_1x1_za16_bf16_bf16' can only be called when SME streaming mode is enabled} }
  svmop4a_za16 (0, bf16, bf16); // { dg-error {ACLE function 'svmop4a_1x1_za16_bf16_bf16' can only be called when SME streaming mode is enabled} }
}

void
error_arg_count_mismatch (svbfloat16_t bf16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_bf16_bf16 (); // { dg-error {too few arguments to function 'svmop4a_1x1_za16_bf16_bf16'; expected 3, have 0} }
  svmop4a_za16 (); // { dg-error {too few arguments to function 'svmop4a_za16'} }

  svmop4a_1x1_za16_bf16_bf16 (0, bf16, bf16, 0); // { dg-error {too many arguments to function 'svmop4a_1x1_za16_bf16_bf16'; expected 3, have 4} }
  svmop4a_za16 (0, bf16, bf16, 0); // { dg-error {too many arguments to function 'svmop4a_za16'} }
}

void
error_arg_type_mismatch (svbfloat16_t bf16, svbfloat16x2_t bf16x2,
			 svbfloat16x4_t bf16x4) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_bf16_bf16 (0, bf16x2, bf16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za16_bf16_bf16'} }
  svmop4a_za16 (0, bf16x4, bf16); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za16_bf16_bf16'} }
}

void
error_zt0_not_immediate (uint64_t zt0,
			 svbfloat16_t bf16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_bf16_bf16 (zt0, bf16, bf16); // { dg-error {argument 1 of 'svmop4a_1x1_za16_bf16_bf16' must be an integer constant expression} }
  svmop4a_za16 (zt0, bf16, bf16); // { dg-error {argument 1 of 'svmop4a_za16' must be an integer constant expression} }
}

void
error_zt0_not_in_range (svbfloat16_t bf16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_bf16_bf16 (-1, bf16, bf16); // { dg-error {passing -1 to argument 1 of 'svmop4a_1x1_za16_bf16_bf16', which expects a value in the range \[0, 1\]} }
  svmop4a_za16 (-1, bf16, bf16); // { dg-error {passing -1 to argument 1 of 'svmop4a_za16', which expects a value in the range \[0, 1\]} }

  svmop4a_1x1_za16_bf16_bf16 (2, bf16, bf16); // { dg-error {passing 2 to argument 1 of 'svmop4a_1x1_za16_bf16_bf16', which expects a value in the range \[0, 1\]} }
  svmop4a_za16 (2, bf16, bf16); // { dg-error {passing 2 to argument 1 of 'svmop4a_za16', which expects a value in the range \[0, 1\]} }
}

#pragma GCC target "+nothing,+sve2,+sme2,+sme-mop4"

void
error_missing_feature (svbfloat16_t bf16) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_bf16_bf16 (0, bf16, bf16); // { dg-error {ACLE function 'svmop4a_1x1_za16_bf16_bf16' requires ISA extension 'sme-b16b16'} }
}
