/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results for vcvt_f64_f32.  */
VECT_VAR_DECL (expected, hfloat, 64, 2) [] = { 0x4030000000000000,
					       0x402e000000000000};
/* Expected results for vcvt_f32_f64.  */
VECT_VAR_DECL (expected, hfloat, 32, 2) [] = { 0x3fc00000, 0x40200000 };

/* Expected results for vcvt_high_f64_f32.  */
VECT_VAR_DECL (expected_high, hfloat, 64, 2) [] = { 0xc02c000000000000,
						    0xc02a000000000000 };
/* Expected results for vcvt_high_f32_f64.  */
VECT_VAR_DECL (expected_high, hfloat, 32, 4) [] = { 0x40000000, 0x40000000,
						    0x3fc00000, 0x40200000 };

void
exec_vcvt (void)
{
  clean_results ();

#define TEST_MSG "vcvt_f64_f32"
  {
    VECT_VAR_DECL (buffer_src, float, 32, 2) [] = { 16.0, 15.0 };

    DECL_VARIABLE (vector_src, float, 32, 2);

    VLOAD (vector_src, buffer_src, , float, f, 32, 2);
    DECL_VARIABLE (vector_res, float, 64, 2) =
	vcvt_f64_f32 (VECT_VAR (vector_src, float, 32, 2));
    vst1q_f64 (VECT_VAR (result, float, 64, 2),
	       VECT_VAR (vector_res, float, 64, 2));

    CHECK_FP (TEST_MSG, float, 64, 2, PRIx64, expected, "");
  }
#undef TEST_MSG

  clean_results ();

#define TEST_MSG "vcvt_f32_f64"
  {
    VECT_VAR_DECL (buffer_src, float, 64, 2) [] = { 1.500000025, 2.500000025 };
    DECL_VARIABLE (vector_src, float, 64, 2);

    VLOAD (vector_src, buffer_src, q, float, f, 64, 2);
    DECL_VARIABLE (vector_res, float, 32, 2) =
      vcvt_f32_f64 (VECT_VAR (vector_src, float, 64, 2));
    vst1_f32 (VECT_VAR (result, float, 32, 2),
	      VECT_VAR (vector_res, float, 32, 2));

    CHECK_FP (TEST_MSG, float, 32, 2, PRIx32, expected, "");
  }
#undef TEST_MSG

  clean_results ();

#define TEST_MSG "vcvt_high_f64_f32"
  {
    DECL_VARIABLE (vector_src, float, 32, 4);
    VLOAD (vector_src, buffer, q, float, f, 32, 4);
    DECL_VARIABLE (vector_res, float, 64, 2);
    VECT_VAR (vector_res, float, 64, 2) =
      vcvt_high_f64_f32 (VECT_VAR (vector_src, float, 32, 4));
    vst1q_f64 (VECT_VAR (result, float, 64, 2),
	       VECT_VAR (vector_res, float, 64, 2));
    CHECK_FP (TEST_MSG, float, 64, 2, PRIx64, expected_high, "");
  }
#undef TEST_MSG

  clean_results ();

#define TEST_MSG "vcvt_high_f32_f64"
  {
    VECT_VAR_DECL (buffer_src, float, 64, 2) [] = { 1.500000025, 2.500000025 };
    DECL_VARIABLE (vector_low, float, 32, 2);
    VDUP (vector_low, , float, f, 32, 2, 2.0);

    DECL_VARIABLE (vector_src, float, 64, 2);
    VLOAD (vector_src, buffer_src, q, float, f, 64, 2);

    DECL_VARIABLE (vector_res, float, 32, 4) =
      vcvt_high_f32_f64 (VECT_VAR (vector_low, float, 32, 2),
			 VECT_VAR (vector_src, float, 64, 2));
    vst1q_f32 (VECT_VAR (result, float, 32, 4),
	       VECT_VAR (vector_res, float, 32, 4));

    CHECK_FP (TEST_MSG, float, 32, 4, PRIx32, expected_high, "");
  }
}

int
main (void)
{
  exec_vcvt ();
  return 0;
}
