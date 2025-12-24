// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

#pragma GCC target "+sve2,+sme-f8f32"
static_assert (__ARM_FEATURE_SME_F8F32 == 1);
#include <arm_sme.h>

/* Available variants are:
   _za32 if __ARM_FEATURE_SME_F8F32 != 0
   void svvdotb_lane_za32[_mf8]_vg1x4_fpm(uint32_t slice, svmfloat8x2_t zn, svmfloat8_t zm, uint64_t imm_idx, fpm_t fpm) __arm_streaming __arm_inout("za");
   void svvdott_lane_za32[_mf8]_vg1x4_fpm(uint32_t slice, svmfloat8x2_t zn, svmfloat8_t zm, uint64_t imm_idx, fpm_t fpm) __arm_streaming __arm_inout("za");  */

void
svvdotb_lane_ok (uint32_t slice, svmfloat8x2_t zn_f8x2, svmfloat8_t zm_f8,
		 fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, fpm);
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, 0, fpm);
}

void
svvdotb_lane_error_not_streaming (uint32_t slice, svmfloat8x2_t zn_f8x2,
				  svmfloat8_t zm_f8,
				  fpm_t fpm) __arm_inout ("za")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' can only be called when SME streaming mode is enabled} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' can only be called when SME streaming mode is enabled} }
}

void
svvdotb_lane_error_streaming_compatible (uint32_t slice, svmfloat8x2_t zn_f8x2,
					 svmfloat8_t zm_f8,
					 fpm_t fpm) __arm_streaming_compatible
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' can only be called when SME streaming mode is enabled} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' can only be called when SME streaming mode is enabled} }
}

void
svvdotb_lane_error_not_inout (uint32_t slice, svmfloat8x2_t zn_f8x2,
			      svmfloat8_t zm_f8, fpm_t fpm) __arm_streaming
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' can only be called from a function that has 'za' state} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' can only be called from a function that has 'za' state} }
}

void
svvdotb_lane_error_arg_count_mismatch (
  uint32_t slice, svmfloat8x2_t zn_f8x2, svmfloat8_t zm_f8,
  fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (); // { dg-error {too few arguments to function 'svvdotb_lane_za32_mf8_vg1x4_fpm'; expected 5, have 0} }
  svvdotb_lane_za32_vg1x4_fpm     (); // { dg-error {too few arguments to function 'svvdotb_lane_za32_vg1x4_fpm'} }

  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, fpm, 0); // { dg-error {too many arguments to function 'svvdotb_lane_za32_mf8_vg1x4_fpm'; expected 5, have 6} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, 0, fpm, 0); // { dg-error {too many arguments to function 'svvdotb_lane_za32_vg1x4_fpm'} }
}

void
svvdotb_lane_error_arg_type_mismatch (
  uint32_t slice, svmfloat8x2_t zn_f8x2, svmfloat8x4_t zn_f8x4,
  svmfloat8_t zm_f8, fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (zm_f8, zn_f8x2, zm_f8, 0, fpm); // { dg-error {incompatible type for argument 1 of 'svvdotb_lane_za32_mf8_vg1x4_fpm'} }
  svvdotb_lane_za32_vg1x4_fpm     (zm_f8, zn_f8x2, zm_f8, 0, fpm); // { dg-error {passing 'svmfloat8_t' to argument 1 of 'svvdotb_lane_za32_vg1x4_fpm', which expects 'uint32_t'} }

  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x4, zm_f8, 0, fpm); // { dg-error {incompatible type for argument 2 of 'svvdotb_lane_za32_mf8_vg1x4_fpm'} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x4, zm_f8, 0, fpm); // { dg-error {passing 'svmfloat8x4_t' to argument 2 of 'svvdotb_lane_za32_vg1x4_fpm', which expects a tuple of 2 vectors} }

  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zn_f8x2, 0, fpm); // { dg-error {incompatible type for argument 3 of 'svvdotb_lane_za32_mf8_vg1x4_fpm'} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zn_f8x2, 0, fpm); // { dg-error {passing 'svmfloat8x2_t' to argument 3 of 'svvdotb_lane_za32_vg1x4_fpm', which expects 'svmfloat8_t'} }

  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, zm_f8, fpm); // { dg-error {incompatible type for argument 4 of 'svvdotb_lane_za32_mf8_vg1x4_fpm'} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, zm_f8, fpm); // { dg-error {argument 4 of 'svvdotb_lane_za32_vg1x4_fpm' must be an integer constant expression} }

  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, zm_f8); // { dg-error {incompatible type for argument 5 of 'svvdotb_lane_za32_mf8_vg1x4_fpm'} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, 0, zm_f8); // { dg-error {incompatible type for argument 5 of 'svvdotb_lane_za32_mf8_vg1x4_fpm'} }
}

void
svvdotb_lane_error_imm_idx_not_immediate (
  uint32_t slice, svmfloat8x2_t zn_f8x2, svmfloat8_t zm_f8, uint64_t imm_idx,
  fpm_t fpm) __arm_streaming __arm_in ("zt0")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, imm_idx, fpm); // { dg-error {argument 4 of 'svvdotb_lane_za32_mf8_vg1x4_fpm' must be an integer constant expression} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, imm_idx, fpm); // { dg-error {argument 4 of 'svvdotb_lane_za32_vg1x4_fpm' must be an integer constant expression} }
}

void
svvdotb_lane_error_imm_idx_not_in_range (
  uint32_t slice, svmfloat8x2_t zn_f8x2, svmfloat8_t zm_f8,
  fpm_t fpm) __arm_streaming __arm_in ("zt0")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, -1, fpm); // { dg-error {passing -1 to argument 4 of 'svvdotb_lane_za32_mf8_vg1x4_fpm', which expects a value in the range \[0, 3\]} }
  svvdotb_lane_za32_vg1x4_fpm     (slice, zn_f8x2, zm_f8, -1, fpm); // { dg-error {passing -1 to argument 4 of 'svvdotb_lane_za32_vg1x4_fpm', which expects a value in the range \[0, 3\]} }
}

#pragma GCC reset_options
#pragma GCC target("+sve2,+sme2")
void
svvdotb_lane_feature_not_enabled (uint32_t slice, svmfloat8x2_t zn_f8x2,
				 svmfloat8_t zm_f8,
				 fpm_t fpm) __arm_streaming __arm_inout ("za")
{
  svvdotb_lane_za32_mf8_vg1x4_fpm (slice, zn_f8x2, zm_f8, 0, fpm); // { dg-error {ACLE function 'svvdotb_lane_za32_mf8_vg1x4_fpm' requires ISA extension 'sme-f8f32'} }
}
