/* { dg-require-effective-target arm_neon_fp16_hw { target { arm*-*-* } } } */
#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results for vcvt.  */
VECT_VAR_DECL (expected,hfloat,32,4) [] = { 0x41800000, 0x41700000,
					    0x41600000, 0x41500000 };
VECT_VAR_DECL (expected,hfloat,16,4) [] = { 0x3e00, 0x4100, 0x4300, 0x4480 };

/* Expected results for vcvt_high_f32_f16.  */
VECT_VAR_DECL (expected_high,hfloat,32,4) [] = { 0xc1400000, 0xc1300000,
						 0xc1200000, 0xc1100000 };
/* Expected results for vcvt_high_f16_f32.  */
VECT_VAR_DECL (expected_high,hfloat,16,8) [] = { 0x4000, 0x4000, 0x4000, 0x4000,
						 0xcc00, 0xcb80, 0xcb00, 0xca80 };

void
exec_vcvt (void)
{
  clean_results ();

#define TEST_MSG "vcvt_f32_f16"
  {
    VECT_VAR_DECL (buffer_src, float, 16, 4) [] = { 16.0, 15.0, 14.0, 13.0 };

    DECL_VARIABLE (vector_src, float, 16, 4);

    VLOAD (vector_src, buffer_src, , float, f, 16, 4);
    DECL_VARIABLE (vector_res, float, 32, 4) =
	vcvt_f32_f16 (VECT_VAR (vector_src, float, 16, 4));
    vst1q_f32 (VECT_VAR (result, float, 32, 4),
	       VECT_VAR (vector_res, float, 32, 4));

    CHECK_FP (TEST_MSG, float, 32, 4, PRIx32, expected, "");
  }
#undef TEST_MSG

  clean_results ();

#define TEST_MSG "vcvt_f16_f32"
  {
    VECT_VAR_DECL (buffer_src, float, 32, 4) [] = { 1.5, 2.5, 3.5, 4.5 };
    DECL_VARIABLE (vector_src, float, 32, 4);

    VLOAD (vector_src, buffer_src, q, float, f, 32, 4);
    DECL_VARIABLE (vector_res, float, 16, 4) =
      vcvt_f16_f32 (VECT_VAR (vector_src, float, 32, 4));
    vst1_f16 (VECT_VAR (result, float, 16, 4),
	      VECT_VAR (vector_res, float, 16 ,4));

    CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected, "");
  }
#undef TEST_MSG

  /* We run more tests for AArch64 as the relevant intrinsics
     do not exist on AArch32.  */
#if defined (__aarch64__)
  clean_results ();

#define TEST_MSG "vcvt_high_f32_f16"
  {
    DECL_VARIABLE (vector_src, float, 16, 8);
    VLOAD (vector_src, buffer, q, float, f, 16, 8);
    DECL_VARIABLE (vector_res, float, 32, 4);
    VECT_VAR (vector_res, float, 32, 4) =
      vcvt_high_f32_f16 (VECT_VAR (vector_src, float, 16, 8));
    vst1q_f32 (VECT_VAR (result, float, 32, 4),
	       VECT_VAR (vector_res, float, 32, 4));
    CHECK_FP (TEST_MSG, float, 32, 4, PRIx32, expected_high, "");
  }
#undef TEST_MSG
  clean_results ();

#define TEST_MSG "vcvt_high_f16_f32"
  {
    DECL_VARIABLE (vector_low, float, 16, 4);
    VDUP (vector_low, , float, f, 16, 4, 2.0);

    DECL_VARIABLE (vector_src, float, 32, 4);
    VLOAD (vector_src, buffer, q, float, f, 32, 4);

    DECL_VARIABLE (vector_res, float, 16, 8) =
      vcvt_high_f16_f32 (VECT_VAR (vector_low, float, 16, 4),
			 VECT_VAR (vector_src, float, 32, 4));
    vst1q_f16 (VECT_VAR (result, float, 16, 8),
	       VECT_VAR (vector_res, float, 16, 8));

    CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_high, "");
  }
#endif
}

int
main (void)
{
  exec_vcvt ();
  return 0;
}
