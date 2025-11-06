// { dg-options "-std=c23 -fsyntax-only" }
// { dg-do compile }

#pragma GCC target "+sve,+sve2,+sme,+sme2,+sve-bfscale"
static_assert (__ARM_FEATURE_SVE2 == 1);
static_assert (__ARM_FEATURE_SME2 == 1);
static_assert (__ARM_FEATURE_SVE_BFSCALE == 1);
#include <arm_sve.h>
#include <arm_sme.h>

/*
- BFSCALE (predicated)
  // Only if __ARM_FEATURE_SVE_BFSCALE != 0 && __ARM_FEATURE_SVE2 != 0
  svbfloat16_t svscale[_bf16]_m (svbool_t pg, svbfloat16_t zdn, svint16_t zm);
  svbfloat16_t svscale[_bf16]_x (svbool_t pg, svbfloat16_t zdn, svint16_t zm);
  svbfloat16_t svscale[_bf16]_z (svbool_t pg, svbfloat16_t zdn, svint16_t zm);
  svbfloat16_t svscale[_n_bf16]_m (svbool_t pg, svbfloat16_t zdn, int16_t zm);
  svbfloat16_t svscale[_n_bf16]_x (svbool_t pg, svbfloat16_t zdn, int16_t zm);
  svbfloat16_t svscale[_n_bf16]_z (svbool_t pg, svbfloat16_t zdn, int16_t zm);  */

void
svscale_predicated_explicit_ok (svbool_t p, svbfloat16_t bf16x1,
				svint16_t i16x1, int16_t i16)
{
  bf16x1 = svscale_bf16_m (p, bf16x1, i16x1);
  bf16x1 = svscale_bf16_x (p, bf16x1, i16x1);
  bf16x1 = svscale_bf16_z (p, bf16x1, i16x1);

  bf16x1 = svscale_n_bf16_m (p, bf16x1, i16);
  bf16x1 = svscale_n_bf16_x (p, bf16x1, i16);
  bf16x1 = svscale_n_bf16_z (p, bf16x1, i16);
}

void
svscale_predicated_inferred_ok (svbool_t p, svbfloat16_t bf16x1,
				svbfloat16x4_t bf16x4, svint16_t i16x1,
				int16_t i16)
{
  bf16x1 = svscale_m (p, bf16x1, i16x1);
  bf16x1 = svscale_x (p, bf16x1, i16x1);
  bf16x1 = svscale_z (p, bf16x1, i16x1);

  bf16x1 = svscale_m (p, bf16x1, i16);
  bf16x1 = svscale_x (p, bf16x1, i16);
  bf16x1 = svscale_z (p, bf16x1, i16);
}

/*
- BFSCALE (multiple vectors)
  // Only if __ARM_FEATURE_SVE_BFSCALE != 0 && __ARM_FEATURE_SME2 != 0
  svbfloat16x2_t svscale[_bf16_x2] (svbfloat16x2_t zdn, svint16x2_t zm) __arm_streaming;
  svbfloat16x4_t svscale[_bf16_x4] (svbfloat16x4_t zdn, svint16x4_t zm) __arm_streaming;

- BFSCALE (multiple and single vector)
  // Only if __ARM_FEATURE_SVE_BFSCALE != 0 && __ARM_FEATURE_SME2 != 0
  svbfloat16x2_t svscale[_single_bf16_x2] (svbfloat16x2_t zn, svint16_t zm) __arm_streaming;
  svbfloat16x4_t svscale[_single_bf16_x4] (svbfloat16x4_t zn, svint16_t zm) __arm_streaming;  */

void
svscale_explicit_ok (svbfloat16_t bf16x1, svbfloat16x2_t bf16x2,
		     svbfloat16x4_t bf16x4, svint16_t i16x1, svint16x2_t i16x2,
		     svint16x4_t i16x4) __arm_streaming
{
  bf16x2 = svscale_bf16_x2 (bf16x2, i16x2);
  bf16x4 = svscale_bf16_x4 (bf16x4, i16x4);

  bf16x2 = svscale_single_bf16_x2 (bf16x2, i16x1);
  bf16x4 = svscale_single_bf16_x4 (bf16x4, i16x1);
}

void
svscale_inferred_ok (svbfloat16x2_t bf16x2, svbfloat16x4_t bf16x4,
		     svint16_t i16x1, svint16x2_t i16x2,
		     svint16x4_t i16x4) __arm_streaming
{
  bf16x2 = svscale_bf16_x2 (bf16x2, i16x2);
  bf16x4 = svscale_bf16_x4 (bf16x4, i16x4);

  bf16x2 = svscale_single_bf16_x2 (bf16x2, i16x1);
  bf16x4 = svscale_single_bf16_x4 (bf16x4, i16x1);
}

/*
- BFMUL (multiple vectors)
  // Only if __ARM_FEATURE_SVE_BFSCALE != 0 && __ARM_FEATURE_SME2 != 0
  svbfloat16x2_t svmul[_bf16_x2] (svbfloat16x2_t zdn, svbfloat16x2_t zm) __arm_streaming;
  svbfloat16x4_t svmul[_bf16_x4] (svbfloat16x4_t zdn, svbfloat16x4_t zm) __arm_streaming;

- BFMUL (multiple and single vector)
  // Only if __ARM_FEATURE_SVE_BFSCALE != 0 && __ARM_FEATURE_SME2 != 0
  svbfloat16x2_t svmul[_single_bf16_x2] (svbfloat16x2_t zn, svbfloat16x2_t zm) __arm_streaming;
  svbfloat16x4_t svmul[_single_bf16_x4] (svbfloat16x4_t zn, svbfloat16x4_t zm) __arm_streaming;  */

void
svmul_explicit_ok (svbfloat16_t bf16x1, svbfloat16x2_t bf16x2,
		   svbfloat16x4_t bf16x4) __arm_streaming
{
  svmul_bf16_x2 (bf16x2, bf16x2);
  svmul_bf16_x4 (bf16x4, bf16x4);

  svmul_single_bf16_x2 (bf16x2, bf16x1);
  svmul_single_bf16_x4 (bf16x4, bf16x1);
}

void
svmul_inferred_ok (svbfloat16_t bf16x1, svbfloat16x2_t bf16x2,
		   svbfloat16x4_t bf16x4) __arm_streaming
{
  svmul (bf16x2, bf16x2);
  svmul (bf16x4, bf16x4);

  svmul (bf16x2, bf16x1);
  svmul (bf16x4, bf16x1);
}
