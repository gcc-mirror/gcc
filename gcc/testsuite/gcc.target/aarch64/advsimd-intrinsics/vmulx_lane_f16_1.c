/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A FP16_C (13.4)
#define B FP16_C (__builtin_inff ())
#define C FP16_C (-34.8)
#define D FP16_C (-__builtin_inff ())
#define E FP16_C (-0.0)
#define F FP16_C (19.1)
#define G FP16_C (-4.8)
#define H FP16_C (0.0)

#define I FP16_C (0.7)
#define J FP16_C (-78)
#define K FP16_C (-__builtin_inff ())
#define L FP16_C (98)
#define M FP16_C (87.1)
#define N FP16_C (-8)
#define O FP16_C (-1.1)
#define P FP16_C (-0.0)

/* Expected results for vmulx_lane.  */
VECT_VAR_DECL (expected0_static, hfloat, 16, 4) []
  = { 0x8000 /* A * E.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * E.  */,
      0x4000 /* FP16_C (2.0f).  */ };

VECT_VAR_DECL (expected1_static, hfloat, 16, 4) []
  = { 0x5BFF /* A * F.  */,
      0x7C00 /* B * F.  */,
      0xE131 /* C * F.  */,
      0xFC00 /* D * F.  */ };

VECT_VAR_DECL (expected2_static, hfloat, 16, 4) []
  = { 0xD405 /* A * G.  */,
      0xFC00 /* B * G.  */,
      0x5939 /* C * G.  */,
      0x7C00 /* D * G.  */ };

VECT_VAR_DECL (expected3_static, hfloat, 16, 4) []
  = { 0x0000 /* A * H.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* C * H.  */,
      0xC000 /* FP16_C (-2.0f).  */ };

/* Expected results for vmulxq_lane.  */
VECT_VAR_DECL (expected0_static, hfloat, 16, 8) []
  = { 0x8000 /* A * E.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * E.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* I * E.  */,
      0x0000 /* J * E.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* L * E.  */ };

VECT_VAR_DECL (expected1_static, hfloat, 16, 8) []
  = { 0x5BFF /* A * F.  */,
      0x7C00 /* B * F.  */,
      0xE131 /* C * F.  */,
      0xFC00 /* D * F.  */,
      0x4AAF /* I * F.  */,
      0xE5D1 /* J * F.  */,
      0xFC00 /* K * F.  */,
      0x674F /* L * F.  */ };

VECT_VAR_DECL (expected2_static, hfloat, 16, 8) []
  = { 0xD405 /* A * G.  */,
      0xFC00 /* B * G.  */,
      0x5939 /* C * G.  */,
      0x7C00 /* D * G.  */,
      0xC2B9 /* I * G.  */,
      0x5DDA /* J * G.  */,
      0x7C00 /* K * G.  */,
      0xDF5A /* L * G.  */ };

VECT_VAR_DECL (expected3_static, hfloat, 16, 8) []
  = { 0x0000 /* A * H.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* C * H.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* I * H.  */,
      0x8000 /* J * H.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* L * H.  */};

/* Expected results for vmulx_laneq.  */
VECT_VAR_DECL (expected_laneq0_static, hfloat, 16, 4) []
  = { 0x8000 /* A * E.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * E.  */,
      0x4000 /* FP16_C (2.0f).  */ };

VECT_VAR_DECL (expected_laneq1_static, hfloat, 16, 4) []
  = { 0x5BFF /* A * F.  */,
      0x7C00 /* B * F.  */,
      0xE131 /* C * F.  */,
      0xFC00 /* D * F.  */ };

VECT_VAR_DECL (expected_laneq2_static, hfloat, 16, 4) []
  = { 0xD405 /* A * G.  */,
      0xFC00 /* B * G.  */,
      0x5939 /* C * G.  */,
      0x7C00 /* D * G.  */ };

VECT_VAR_DECL (expected_laneq3_static, hfloat, 16, 4) []
  = { 0x0000 /* A * H.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* C * H.  */,
      0xC000 /* FP16_C (-2.0f).  */ };

VECT_VAR_DECL (expected_laneq4_static, hfloat, 16, 4) []
  = { 0x648F /* A * M.  */,
      0x7C00 /* B * M.  */,
      0xE9ED /* C * M.  */,
      0xFC00 /* D * M.  */ };

VECT_VAR_DECL (expected_laneq5_static, hfloat, 16, 4) []
  = { 0xD6B3 /* A * N.  */,
      0xFC00 /* B * N.  */,
      0x5C5A /* C * N.  */,
      0x7C00 /* D * N.  */ };

VECT_VAR_DECL (expected_laneq6_static, hfloat, 16, 4) []
  = { 0xCB5E /* A * O.  */,
      0xFC00 /* B * O.  */,
      0x50C9 /* C * O.  */,
      0x7C00 /* D * O.  */ };

VECT_VAR_DECL (expected_laneq7_static, hfloat, 16, 4) []
  = { 0x8000 /* A * P.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * P.  */,
      0x4000 /* FP16_C (2.0f).  */ };

VECT_VAR_DECL (expected_laneq0_static, hfloat, 16, 8) []
  = { 0x8000 /* A * E.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * E.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* I * E.  */,
      0x0000 /* J * E.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* L * E.  */  };

VECT_VAR_DECL (expected_laneq1_static, hfloat, 16, 8) []
  = { 0x5BFF /* A * F.  */,
      0x7C00 /* B * F.  */,
      0xE131 /* C * F.  */,
      0xFC00 /* D * F.  */,
      0x4AAF /* I * F.  */,
      0xE5D1 /* J * F.  */,
      0xFC00 /* K * F.  */,
      0x674F /* L * F.  */ };

VECT_VAR_DECL (expected_laneq2_static, hfloat, 16, 8) []
  = { 0xD405 /* A * G.  */,
      0xFC00 /* B * G.  */,
      0x5939 /* C * G.  */,
      0x7C00 /* D * G.  */,
      0xC2B9 /* I * G.  */,
      0x5DDA /* J * G.  */,
      0x7C00 /* K * G.  */,
      0xDF5A /* L * G.  */ };

VECT_VAR_DECL (expected_laneq3_static, hfloat, 16, 8) []
  = { 0x0000 /* A * H.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* C * H.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* I * H.  */,
      0x8000 /* J * H.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* L * H.  */ };

VECT_VAR_DECL (expected_laneq4_static, hfloat, 16, 8) []
  = { 0x648F /* A * M.  */,
      0x7C00 /* B * M.  */,
      0xE9ED /* C * M.  */,
      0xFC00 /* D * M.  */,
      0x53A0 /* I * M.  */,
      0xEEA3 /* J * M.  */,
      0xFC00 /* K * M.  */,
      0x702B /* L * M.  */ };

VECT_VAR_DECL (expected_laneq5_static, hfloat, 16, 8) []
  = { 0xD6B3 /* A * N.  */,
      0xFC00 /* B * N.  */,
      0x5C5A /* C * N.  */,
      0x7C00 /* D * N.  */,
      0xC59A /* I * N.  */,
      0x60E0 /* J * N.  */,
      0x7C00 /* K * N.  */,
      0xE220 /* L * N.  */ };

VECT_VAR_DECL (expected_laneq6_static, hfloat, 16, 8) []
  = { 0xCB5E /* A * O.  */,
      0xFC00 /* B * O.  */,
      0x50C9 /* C * O.  */,
      0x7C00 /* D * O.  */,
      0xBA29 /* I * O.  */,
      0x555C /* J * O.  */,
      0x7C00 /* K * O.  */,
      0xD6BC /* L * O.  */ };

VECT_VAR_DECL (expected_laneq7_static, hfloat, 16, 8) []
  = { 0x8000 /* A * P.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * P.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* I * P.  */,
      0x0000 /* J * P.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* L * P.  */ };

void exec_vmulx_lane_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VMULX_LANE (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 4);
  DECL_VARIABLE(vsrc_2, float, 16, 4);
  VECT_VAR_DECL (buf_src_1, float, 16, 4) [] = {A, B, C, D};
  VECT_VAR_DECL (buf_src_2, float, 16, 4) [] = {E, F, G, H};
  VLOAD (vsrc_1, buf_src_1, , float, f, 16, 4);
  VLOAD (vsrc_2, buf_src_2, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vmulx_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 4), 0);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 4), 1);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 4), 2);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 4), 3);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected3_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULXQ_LANE (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 8);
  VECT_VAR_DECL (buf_src_1, float, 16, 8) [] = {A, B, C, D, I, J, K, L};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vmulxq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 4), 0);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 4), 1);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 4), 2);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 4), 3);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected3_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULX_LANEQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_2, float, 16, 8);
  VECT_VAR_DECL (buf_src_2, float, 16, 8) [] = {E, F, G, H, M, N, O, P};
  VLOAD (vsrc_2, buf_src_2, q, float, f, 16, 8);
  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 0);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 1);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 2);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 3);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq3_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 4);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq4_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 5);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq5_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 6);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq6_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		       VECT_VAR (vsrc_2, float, 16, 8), 7);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq7_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULXQ_LANEQ (FP16)"
  clean_results ();

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 0);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 1);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 2);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 3);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq3_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 4);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq4_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 5);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq5_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 6);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq6_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
			VECT_VAR (vsrc_2, float, 16, 8), 7);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq7_static, "");
}

int
main (void)
{
  exec_vmulx_lane_f16 ();
  return 0;
}
