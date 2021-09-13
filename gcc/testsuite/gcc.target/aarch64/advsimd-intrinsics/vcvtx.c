/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results for vcvtx_f32_f64 and vcvtxd_f32_f64.  */
VECT_VAR_DECL (expected, hfloat, 32, 2) [] = { 0x3fc00001, 0x40200001 };

/* Expected results for vcvtx_high_f32_f64.  */
VECT_VAR_DECL (expected_high, hfloat, 32, 4) [] = { 0x40000000, 0x40000000,
						    0x3fc00001, 0x40200001 };

void
exec_vcvtx (void)
{
  clean_results ();

#define TEST_MSG "vcvtx_f32_f64"
  {
    VECT_VAR_DECL (buffer_src, float, 64, 2) [] = { 1.500000025, 2.500000025 };
    DECL_VARIABLE (vector_src, float, 64, 2);

    VLOAD (vector_src, buffer_src, q, float, f, 64, 2);
    DECL_VARIABLE (vector_res, float, 32, 2) =
      vcvtx_f32_f64 (VECT_VAR (vector_src, float, 64, 2));
    vst1_f32 (VECT_VAR (result, float, 32, 2),
	      VECT_VAR (vector_res, float, 32, 2));

    CHECK_FP (TEST_MSG, float, 32, 2, PRIx32, expected, "");
  }
#undef TEST_MSG

  clean_results ();

#define TEST_MSG "vcvtxd_f32_f64"
  {
    DECL_VARIABLE (vector_src, float, 32, 2);
    VDUP (vector_src, , float, f, 32, 2, 0.0);

    DECL_VARIABLE (vector_res, float, 32, 2) =
      vset_lane_f32(vcvtxd_f32_f64 (1.500000025),
		    VECT_VAR (vector_src, float, 32, 2),
		    0);
    VECT_VAR (vector_res, float, 32, 2) =
      vset_lane_f32(vcvtxd_f32_f64 (2.500000025),
		    VECT_VAR (vector_res, float, 32, 2),
		    1);
    vst1_f32 (VECT_VAR (result, float, 32, 2),
	      VECT_VAR (vector_res, float, 32, 2));

    CHECK_FP (TEST_MSG, float, 32, 2, PRIx32, expected, "");
  }
#undef TEST_MSG

  clean_results ();

#define TEST_MSG "vcvtx_high_f32_f64"
  {
    VECT_VAR_DECL (buffer_src, float, 64, 2) [] = { 1.500000025, 2.500000025 };
    DECL_VARIABLE (vector_low, float, 32, 2);
    VDUP (vector_low, , float, f, 32, 2, 2.0);

    DECL_VARIABLE (vector_src, float, 64, 2);
    VLOAD (vector_src, buffer_src, q, float, f, 64, 2);

    DECL_VARIABLE (vector_res, float, 32, 4) =
      vcvtx_high_f32_f64 (VECT_VAR (vector_low, float, 32, 2),
			  VECT_VAR (vector_src, float, 64, 2));
    vst1q_f32 (VECT_VAR (result, float, 32, 4),
	       VECT_VAR (vector_res, float, 32, 4));

    CHECK_FP (TEST_MSG, float, 32, 4, PRIx32, expected_high, "");
  }
}

int
main (void)
{
  exec_vcvtx ();
  return 0;
}
