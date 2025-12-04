// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

// svmop4a[_1x1]_za64[_f64_f64] (only if __ARM_FEATURE_SME_F64F64 != 0)

#pragma GCC target "+sve2,+sme-mop4,+sme-f64f64"
static_assert (__ARM_FEATURE_SME_MOP4 == 1);
static_assert (__ARM_FEATURE_SME_F64F64 == 1);
#include <arm_sme.h>

void
explicit_ok (svfloat64_t f64) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_f64_f64 (0, f64, f64);
}

void
implicit_ok (svfloat64_t f64) __arm_streaming __arm_inout ("za")
{
  svmop4a_za64 (0, f64, f64);
}

void
error_not_streaming (svfloat64_t f64)
{
  svmop4a_1x1_za64_f64_f64 (0, f64, f64); // { dg-error {ACLE function 'svmop4a_1x1_za64_f64_f64' can only be called when SME streaming mode is enabled} }
  svmop4a_za64 (0, f64, f64); // { dg-error {ACLE function 'svmop4a_1x1_za64_f64_f64' can only be called when SME streaming mode is enabled} }
}

void
error_streaming_compatible (svfloat64_t f64) __arm_streaming_compatible
{
  svmop4a_1x1_za64_f64_f64 (0, f64, f64); // { dg-error {ACLE function 'svmop4a_1x1_za64_f64_f64' can only be called when SME streaming mode is enabled} }
  svmop4a_za64 (0, f64, f64); // { dg-error {ACLE function 'svmop4a_1x1_za64_f64_f64' can only be called when SME streaming mode is enabled} }
}

void
error_arg_count_mismatch (svfloat64_t f64) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_f64_f64 (); // { dg-error {too few arguments to function 'svmop4a_1x1_za64_f64_f64'; expected 3, have 0} }
  svmop4a_za64 (); // { dg-error {too few arguments to function 'svmop4a_za64'} }

  svmop4a_1x1_za64_f64_f64 (0, f64, f64, 0); // { dg-error {too many arguments to function 'svmop4a_1x1_za64_f64_f64'; expected 3, have 4} }
  svmop4a_za64 (0, f64, f64, 0); // { dg-error {too many arguments to function 'svmop4a_za64'} }
}

void
error_arg_type_mismatch (svfloat64_t f64, svfloat64x2_t f64x2,
			 svfloat64x4_t f64x4) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_f64_f64 (0, f64x2, f64); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za64_f64_f64'} }
  svmop4a_za64 (0, f64x4, f64); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za64_f64_f64'} }
}

void
error_zt0_not_immediate (uint64_t zt0,
			 svfloat64_t f64) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_f64_f64 (zt0, f64, f64); // { dg-error {argument 1 of 'svmop4a_1x1_za64_f64_f64' must be an integer constant expression} }
  svmop4a_za64 (zt0, f64, f64); // { dg-error {argument 1 of 'svmop4a_za64' must be an integer constant expression} }
}

void
error_zt0_not_in_range (svfloat64_t f64) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_f64_f64 (-1, f64, f64); // { dg-error {passing -1 to argument 1 of 'svmop4a_1x1_za64_f64_f64', which expects a value in the range \[0, 7\]} }
  svmop4a_za64 (-1, f64, f64); // { dg-error {passing -1 to argument 1 of 'svmop4a_za64', which expects a value in the range \[0, 7\]} }

  svmop4a_1x1_za64_f64_f64 (8, f64, f64); // { dg-error {passing 8 to argument 1 of 'svmop4a_1x1_za64_f64_f64', which expects a value in the range \[0, 7\]} }
  svmop4a_za64 (8, f64, f64); // { dg-error {passing 8 to argument 1 of 'svmop4a_za64', which expects a value in the range \[0, 7\]} }
}

#pragma GCC target "+nothing,+sve2,+sme2,+sme-mop4"

void
error_missing_feature (svfloat64_t f64) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za64_f64_f64 (0, f64, f64); // { dg-error {ACLE function 'svmop4a_1x1_za64_f64_f64' requires ISA extension 'sme-f64f64'} }
}
