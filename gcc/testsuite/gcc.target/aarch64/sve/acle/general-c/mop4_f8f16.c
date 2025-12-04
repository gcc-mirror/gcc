// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

// svmop4a[_1x1]_za16[_mf8_mf8]_fpm (only if __ARM_FEATURE_SME_F8F16 != 0)

#pragma GCC target "+sve2,+sme-mop4,+sme-f8f16"
static_assert (__ARM_FEATURE_SME_MOP4 == 1);
static_assert (__ARM_FEATURE_SME_F8F16 == 1);
#include <arm_sme.h>

void
explicit_ok (svmfloat8_t mf8, fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_mf8_mf8_fpm (0, mf8, mf8, fpm);
}

void
implicit_ok (svmfloat8_t mf8, fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_za16_fpm (0, mf8, mf8, fpm);
}

void
error_not_streaming (svmfloat8_t mf8, fpm_t fpm)
{
  svmop4a_1x1_za16_mf8_mf8_fpm (0, mf8, mf8, fpm); // { dg-error {ACLE function 'svmop4a_1x1_za16_mf8_mf8_fpm' can only be called when SME streaming mode is enabled} }
  svmop4a_za16_fpm (0, mf8, mf8, fpm); // { dg-error {ACLE function 'svmop4a_1x1_za16_mf8_mf8_fpm' can only be called when SME streaming mode is enabled} }
}

void
error_streaming_compatible (svmfloat8_t mf8,
			    fpm_t fpm) __arm_streaming_compatible
{
  svmop4a_1x1_za16_mf8_mf8_fpm (0, mf8, mf8, fpm); // { dg-error {ACLE function 'svmop4a_1x1_za16_mf8_mf8_fpm' can only be called when SME streaming mode is enabled} }
  svmop4a_za16_fpm (0, mf8, mf8, fpm); // { dg-error {ACLE function 'svmop4a_1x1_za16_mf8_mf8_fpm' can only be called when SME streaming mode is enabled} }
}

void
error_arg_count_mismatch (svmfloat8_t mf8,
			  fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_mf8_mf8_fpm (); // { dg-error {too few arguments to function 'svmop4a_1x1_za16_mf8_mf8_fpm'; expected 4, have 0} }
  svmop4a_za16_fpm (); // { dg-error {too few arguments to function 'svmop4a_za16_fpm'} }

  svmop4a_1x1_za16_mf8_mf8_fpm (0, mf8, mf8, fpm, 0); // { dg-error {too many arguments to function 'svmop4a_1x1_za16_mf8_mf8_fpm'; expected 4, have 5} }
  svmop4a_za16_fpm (0, mf8, mf8, fpm, 0); // { dg-error {too many arguments to function 'svmop4a_za16_fpm'} }
}

void
error_arg_type_mismatch (svmfloat8_t mf8, svmfloat8x2_t mf8x2,
			 svmfloat8x4_t mf8x4,
			 fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_mf8_mf8_fpm (0, mf8x2, mf8, fpm); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za16_mf8_mf8_fpm'} }
  svmop4a_za16_fpm (0, mf8x4, mf8, fpm); // { dg-error {incompatible type for argument 2 of 'svmop4a_1x1_za16_mf8_mf8_fpm'} }
}

void
error_zt0_not_immediate (uint64_t zt0, svmfloat8_t mf8,
			 fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_mf8_mf8_fpm (zt0, mf8, mf8, fpm); // { dg-error {argument 1 of 'svmop4a_1x1_za16_mf8_mf8_fpm' must be an integer constant expression} }
  svmop4a_za16_fpm (zt0, mf8, mf8, fpm); // { dg-error {argument 1 of 'svmop4a_za16_fpm' must be an integer constant expression} }
}

void
error_zt0_not_in_range (svmfloat8_t mf8,
			fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_mf8_mf8_fpm (-1, mf8, mf8, fpm); // { dg-error {passing -1 to argument 1 of 'svmop4a_1x1_za16_mf8_mf8_fpm', which expects a value in the range \[0, 1\]} }
  svmop4a_za16_fpm (-1, mf8, mf8, fpm); // { dg-error {passing -1 to argument 1 of 'svmop4a_za16_fpm', which expects a value in the range \[0, 1\]} }

  svmop4a_1x1_za16_mf8_mf8_fpm (2, mf8, mf8, fpm); // { dg-error {passing 2 to argument 1 of 'svmop4a_1x1_za16_mf8_mf8_fpm', which expects a value in the range \[0, 1\]} }
  svmop4a_za16_fpm (2, mf8, mf8, fpm); // { dg-error {passing 2 to argument 1 of 'svmop4a_za16_fpm', which expects a value in the range \[0, 1\]} }
}

#pragma GCC target "+nothing,+sve2,+sme2,+sme-mop4"

void
error_missing_feature (svmfloat8_t mf8,
		       fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svmop4a_1x1_za16_mf8_mf8_fpm (0, mf8, mf8, fpm); // { dg-error {ACLE function 'svmop4a_1x1_za16_mf8_mf8_fpm' requires ISA extension 'sme-f8f16'} }
}
