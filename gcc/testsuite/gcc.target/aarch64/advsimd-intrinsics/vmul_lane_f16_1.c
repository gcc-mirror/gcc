/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A FP16_C (13.4)
#define B FP16_C (-56.8)
#define C FP16_C (-34.8)
#define D FP16_C (12)
#define E FP16_C (63.1)
#define F FP16_C (19.1)
#define G FP16_C (-4.8)
#define H FP16_C (77)

#define I FP16_C (0.7)
#define J FP16_C (-78)
#define K FP16_C (11.23)
#define L FP16_C (98)
#define M FP16_C (87.1)
#define N FP16_C (-8)
#define O FP16_C (-1.1)
#define P FP16_C (-9.7)

/* Expected results for vmul_lane.  */
VECT_VAR_DECL (expected0_static, hfloat, 16, 4) []
  = { 0x629B /* A * E.  */,
      0xEB00 /* B * E.  */,
      0xE84A /* C * E.  */,
      0x61EA /* D * E.  */ };

VECT_VAR_DECL (expected1_static, hfloat, 16, 4) []
  = { 0x5BFF /* A * F.  */,
      0xE43D /* B * F.  */,
      0xE131 /* C * F.  */,
      0x5B29 /* D * F.  */ };

VECT_VAR_DECL (expected2_static, hfloat, 16, 4) []
  = { 0xD405 /* A * G.  */,
      0x5C43 /* B * G.  */,
      0x5939 /* C * G.  */,
      0xD334 /* D * G.  */ };

VECT_VAR_DECL (expected3_static, hfloat, 16, 4) []
  = { 0x6408 /* A * H.  */,
      0xEC46 /* B * H.  */,
      0xE93C /* C * H.  */,
      0x6338 /* D * H.  */ };

/* Expected results for vmulq_lane.  */
VECT_VAR_DECL (expected0_static, hfloat, 16, 8) []
  = { 0x629B /* A * E.  */,
      0xEB00 /* B * E.  */,
      0xE84A /* C * E.  */,
      0x61EA /* D * E.  */,
      0x5186 /* I * E.  */,
      0xECCE /* J * E.  */,
      0x6189 /* K * E.  */,
      0x6E0A /* L * E.  */ };

VECT_VAR_DECL (expected1_static, hfloat, 16, 8) []
  = { 0x5BFF /* A * F.  */,
      0xE43D /* B * F.  */,
      0xE131 /* C * F.  */,
      0x5B29 /* D * F.  */,
      0x4AAF /* I * F.  */,
      0xE5D1 /* J * F.  */,
      0x5AB3 /* K * F.  */,
      0x674F /* L * F.  */ };

VECT_VAR_DECL (expected2_static, hfloat, 16, 8) []
  = { 0xD405 /* A * G.  */,
      0x5C43 /* B * G.  */,
      0x5939 /* C * G.  */,
      0xD334 /* D * G.  */,
      0xC2B9 /* I * G.  */,
      0x5DDA /* J * G.  */,
      0xD2BD /* K * G.  */,
      0xDF5A /* L * G.  */ };

VECT_VAR_DECL (expected3_static, hfloat, 16, 8) []
  = { 0x6408 /* A * H.  */,
      0xEC46 /* B * H.  */,
      0xE93C /* C * H.  */,
      0x6338 /* D * H.  */,
      0x52BD /* I * H.  */,
      0xEDDE /* J * H.  */,
      0x62C1 /* K * H.  */,
      0x6F5E /* L * H.  */ };

/* Expected results for vmul_laneq.  */
VECT_VAR_DECL (expected_laneq0_static, hfloat, 16, 4) []
  = { 0x629B /* A * E.  */,
      0xEB00 /* B * E.  */,
      0xE84A /* C * E.  */,
      0x61EA /* D * E.  */ };

VECT_VAR_DECL (expected_laneq1_static, hfloat, 16, 4) []
  = { 0x5BFF /* A * F.  */,
      0xE43D /* B * F.  */,
      0xE131 /* C * F.  */,
      0x5B29 /* D * F.  */ };

VECT_VAR_DECL (expected_laneq2_static, hfloat, 16, 4) []
  = { 0xD405 /* A * G.  */,
      0x5C43 /* B * G.  */,
      0x5939 /* C * G.  */,
      0xD334 /* D * G.  */ };

VECT_VAR_DECL (expected_laneq3_static, hfloat, 16, 4) []
  = { 0x6408 /* A * H.  */,
      0xEC46 /* B * H.  */,
      0xE93C /* C * H.  */,
      0x6338 /* D * H.  */ };

VECT_VAR_DECL (expected_laneq4_static, hfloat, 16, 4) []
  = { 0x648F /* A * M.  */,
      0xECD5 /* B * M.  */,
      0xE9ED /* C * M.  */,
      0x6416 /* D * M.  */ };

VECT_VAR_DECL (expected_laneq5_static, hfloat, 16, 4) []
  = { 0xD6B3 /* A * N.  */,
      0x5F1A /* B * N.  */,
      0x5C5A /* C * N.  */,
      0xD600 /* D * N.  */ };

VECT_VAR_DECL (expected_laneq6_static, hfloat, 16, 4) []
  = { 0xCB5E /* A * O.  */,
      0x53CF /* B * O.  */,
      0x50C9 /* C * O.  */,
      0xCA99 /* D * O.  */ };

VECT_VAR_DECL (expected_laneq7_static, hfloat, 16, 4) []
  = { 0xD810 /* A * P.  */,
      0x604F /* B * P.  */,
      0x5D47 /* C * P.  */,
      0xD747 /* D * P.  */ };

/* Expected results for vmulq_laneq.  */
VECT_VAR_DECL (expected_laneq0_static, hfloat, 16, 8) []
  = { 0x629B /* A * E.  */,
      0xEB00 /* B * E.  */,
      0xE84A /* C * E.  */,
      0x61EA /* D * E.  */,
      0x5186 /* I * E.  */,
      0xECCE /* J * E.  */,
      0x6189 /* K * E.  */,
      0x6E0A /* L * E.  */ };

VECT_VAR_DECL (expected_laneq1_static, hfloat, 16, 8) []
  = { 0x5BFF /* A * F.  */,
      0xE43D /* B * F.  */,
      0xE131 /* C * F.  */,
      0x5B29 /* D * F.  */,
      0x4AAF /* I * F.  */,
      0xE5D1 /* J * F.  */,
      0x5AB3 /* K * F.  */,
      0x674F /* L * F.  */ };

VECT_VAR_DECL (expected_laneq2_static, hfloat, 16, 8) []
  = { 0xD405 /* A * G.  */,
      0x5C43 /* B * G.  */,
      0x5939 /* C * G.  */,
      0xD334 /* D * G.  */,
      0xC2B9 /* I * G.  */,
      0x5DDA /* J * G.  */,
      0xD2BD /* K * G.  */,
      0xDF5A /* L * G.  */ };

VECT_VAR_DECL (expected_laneq3_static, hfloat, 16, 8) []
  = { 0x6408 /* A * H.  */,
      0xEC46 /* B * H.  */,
      0xE93C /* C * H.  */,
      0x6338 /* D * H.  */,
      0x52BD /* I * H.  */,
      0xEDDE /* J * H.  */,
      0x62C1 /* K * H.  */,
      0x6F5E /* L * H.  */ };

VECT_VAR_DECL (expected_laneq4_static, hfloat, 16, 8) []
  = { 0x648F /* A * M.  */,
      0xECD5 /* B * M.  */,
      0xE9ED /* C * M.  */,
      0x6416 /* D * M.  */,
      0x53A0 /* I * M.  */,
      0xEEA3 /* J * M.  */,
      0x63A4 /* K * M.  */,
      0x702B /* L * M.  */ };

VECT_VAR_DECL (expected_laneq5_static, hfloat, 16, 8) []
  = { 0xD6B3 /* A * N.  */,
      0x5F1A /* B * N.  */,
      0x5C5A /* C * N.  */,
      0xD600 /* D * N.  */,
      0xC59A /* I * N.  */,
      0x60E0 /* J * N.  */,
      0xD59D /* K * N.  */,
      0xE220 /* L * N.  */ };

VECT_VAR_DECL (expected_laneq6_static, hfloat, 16, 8) []
  = { 0xCB5E /* A * O.  */,
      0x53CF /* B * O.  */,
      0x50C9 /* C * O.  */,
      0xCA99 /* D * O.  */,
      0xBA29 /* I * O.  */,
      0x555C /* J * O.  */,
      0xCA2C /* K * O.  */,
      0xD6BC /* L * O.  */ };

VECT_VAR_DECL (expected_laneq7_static, hfloat, 16, 8) []
  = { 0xD810 /* A * P.  */,
      0x604F /* B * P.  */,
      0x5D47 /* C * P.  */,
      0xD747 /* D * P.  */,
      0xC6CB /* I * P.  */,
      0x61EA /* J * P.  */,
      0xD6CF /* K * P.  */,
      0xE36E /* L * P.  */ };

void exec_vmul_lane_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VMUL_LANE (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 4);
  DECL_VARIABLE(vsrc_2, float, 16, 4);
  VECT_VAR_DECL (buf_src_1, float, 16, 4) [] = {A, B, C, D};
  VECT_VAR_DECL (buf_src_2, float, 16, 4) [] = {E, F, G, H};
  VLOAD (vsrc_1, buf_src_1, , float, f, 16, 4);
  VLOAD (vsrc_2, buf_src_2, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vmul_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		     VECT_VAR (vsrc_2, float, 16, 4), 0);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		     VECT_VAR (vsrc_2, float, 16, 4), 1);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		     VECT_VAR (vsrc_2, float, 16, 4), 2);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_lane_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		     VECT_VAR (vsrc_2, float, 16, 4), 3);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected3_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULQ_LANE (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 8);
  VECT_VAR_DECL (buf_src_1, float, 16, 8) [] = {A, B, C, D, I, J, K, L};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vmulq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		      VECT_VAR (vsrc_2, float, 16, 4), 0);

  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		      VECT_VAR (vsrc_2, float, 16, 4), 1);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		      VECT_VAR (vsrc_2, float, 16, 4), 2);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_lane_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		      VECT_VAR (vsrc_2, float, 16, 4), 3);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected3_static, "");

#undef TEST_MSG
#define TEST_MSG "VMUL_LANEQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_2, float, 16, 8);
  VECT_VAR_DECL (buf_src_2, float, 16, 8) [] = {E, F, G, H, M, N, O, P};
  VLOAD (vsrc_2, buf_src_2, q, float, f, 16, 8);
  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 0);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 1);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 2);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 3);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq3_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 4);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq4_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 5);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq5_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 6);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq6_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmul_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		      VECT_VAR (vsrc_2, float, 16, 8), 7);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_laneq7_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULQ_LANEQ (FP16)"
  clean_results ();

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 0);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 1);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 2);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 3);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq3_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 4);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq4_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 5);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq5_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 6);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq6_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulq_laneq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		       VECT_VAR (vsrc_2, float, 16, 8), 7);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_laneq7_static, "");
}

int
main (void)
{
  exec_vmul_lane_f16 ();
  return 0;
}
