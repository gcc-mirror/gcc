/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A0 FP16_C (123.4)
#define A1 FP16_C (-5.8)
#define A2 FP16_C (-0.0)
#define A3 FP16_C (10)
#define A4 FP16_C (123412.43)
#define A5 FP16_C (-5.8)
#define A6 FP16_C (90.8)
#define A7 FP16_C (24)

#define B0 FP16_C (23.4)
#define B1 FP16_C (-5.8)
#define B2 FP16_C (8.9)
#define B3 FP16_C (4.0)
#define B4 FP16_C (3.4)
#define B5 FP16_C (-550.8)
#define B6 FP16_C (-31.8)
#define B7 FP16_C (20000.0)

/* Expected results for vfma_n.  */
VECT_VAR_DECL (expected_fma0_static, hfloat, 16, 4) []
  = { 0x613E /* A0 + B0 * B0.  */,
      0xD86D /* A1 + B1 * B0.  */,
      0x5A82 /* A2 + B2 * B0.  */,
      0x567A /* A3 + B3 * B0.  */ };

VECT_VAR_DECL (expected_fma1_static, hfloat, 16, 4) []
  = { 0xCA33 /* A0 + B0 * B1.  */,
      0x4EF6 /* A1 + B1 * B1.  */,
      0xD274 /* A2 + B2 * B1.  */,
      0xCA9A /* A3 + B3 * B1.  */ };

VECT_VAR_DECL (expected_fma2_static, hfloat, 16, 4) []
  = { 0x5D2F /* A0 + B0 * B2.  */,
      0xD32D /* A1 + B1 * B2.  */,
      0x54F3 /* A2 + B2 * B2.  */,
      0x51B3 /* A3 + B3 * B2.  */ };

VECT_VAR_DECL (expected_fma3_static, hfloat, 16, 4) []
  = { 0x5AC8 /* A0 + B0 * B3.  */,
      0xCF40 /* A1 + B1 * B3.  */,
      0x5073 /* A2 + B2 * B3.  */,
      0x4E80 /* A3 + B3 * B3.  */ };

VECT_VAR_DECL (expected_fma0_static, hfloat, 16, 8) []
  = { 0x613E /* A0 + B0 * B0.  */,
      0xD86D /* A1 + B1 * B0.  */,
      0x5A82 /* A2 + B2 * B0.  */,
      0x567A /* A3 + B3 * B0.  */,
      0x7C00 /* A4 + B4 * B0.  */,
      0xF24D /* A5 + B5 * B0.  */,
      0xE11B /* A6 + B6 * B0.  */,
      0x7C00 /* A7 + B7 * B0.  */ };

VECT_VAR_DECL (expected_fma1_static, hfloat, 16, 8) []
  = { 0xCA33 /* A0 + B0 * B1.  */,
      0x4EF6 /* A1 + B1 * B1.  */,
      0xD274 /* A2 + B2 * B1.  */,
      0xCA9A /* A3 + B3 * B1.  */,
      0x7C00 /* A4 + B4 * B1.  */,
      0x6A3B /* A5 + B5 * B1.  */,
      0x5C4D /* A6 + B6 * B1.  */,
      0xFC00 /* A7 + B7 * B1.  */ };

VECT_VAR_DECL (expected_fma2_static, hfloat, 16, 8) []
  = { 0x5D2F /* A0 + B0 * B2.  */,
      0xD32D /* A1 + B1 * B2.  */,
      0x54F3 /* A2 + B2 * B2.  */,
      0x51B3 /* A3 + B3 * B2.  */,
      0x7C00 /* A4 + B4 * B2.  */,
      0xECCB /* A5 + B5 * B2.  */,
      0xDA01 /* A6 + B6 * B2.  */,
      0x7C00 /* A7 + B7 * B2.  */ };

VECT_VAR_DECL (expected_fma3_static, hfloat, 16, 8) []
  = { 0x5AC8 /* A0 + B0 * B3.  */,
      0xCF40 /* A1 + B1 * B3.  */,
      0x5073 /* A2 + B2 * B3.  */,
      0x4E80 /* A3 + B3 * B3.  */,
      0x7C00 /* A4 + B4 * B3.  */,
      0xE851 /* A5 + B5 * B3.  */,
      0xD08C /* A6 + B6 * B3.  */,
      0x7C00 /* A7 + B7 * B3.  */ };

VECT_VAR_DECL (expected_fma4_static, hfloat, 16, 8) []
  = { 0x5A58 /* A0 + B0 * B4.  */,
      0xCE62 /* A1 + B1 * B4.  */,
      0x4F91 /* A2 + B2 * B4.  */,
      0x4DE6 /* A3 + B3 * B4.  */,
      0x7C00 /* A4 + B4 * B4.  */,
      0xE757 /* A5 + B5 * B4.  */,
      0xCC54 /* A6 + B6 * B4.  */,
      0x7C00 /* A7 + B7 * B4.  */ };

VECT_VAR_DECL (expected_fma5_static, hfloat, 16, 8) []
  = { 0xF23D /* A0 + B0 * B5.  */,
      0x6A3B /* A1 + B1 * B5.  */,
      0xECCA /* A2 + B2 * B5.  */,
      0xE849 /* A3 + B3 * B5.  */,
      0x7C00 /* A4 + B4 * B5.  */,
      0x7C00 /* A5 + B5 * B5.  */,
      0x744D /* A6 + B6 * B5.  */,
      0xFC00 /* A7 + B7 * B5.  */ };

VECT_VAR_DECL (expected_fma6_static, hfloat, 16, 8) []
  = { 0xE0DA /* A0 + B0 * B6.  */,
      0x5995 /* A1 + B1 * B6.  */,
      0xDC6C /* A2 + B2 * B6.  */,
      0xD753 /* A3 + B3 * B6.  */,
      0x7C00 /* A4 + B4 * B6.  */,
      0x7447 /* A5 + B5 * B6.  */,
      0x644E /* A6 + B6 * B6.  */,
      0xFC00 /* A7 + B7 * B6.  */ };

VECT_VAR_DECL (expected_fma7_static, hfloat, 16, 8) []
  = { 0x7C00 /* A0 + B0 * B7.  */,
      0xFC00 /* A1 + B1 * B7.  */,
      0x7C00 /* A2 + B2 * B7.  */,
      0x7C00 /* A3 + B3 * B7.  */,
      0x7C00 /* A4 + B4 * B7.  */,
      0xFC00 /* A5 + B5 * B7.  */,
      0xFC00 /* A6 + B6 * B7.  */,
      0x7C00 /* A7 + B7 * B7.  */ };

/* Expected results for vfms_n.  */
VECT_VAR_DECL (expected_fms0_static, hfloat, 16, 4) []
  = { 0xDEA2 /* A0 + (-B0) * B0.  */,
      0x5810 /* A1 + (-B1) * B0.  */,
      0xDA82 /* A2 + (-B2) * B0.  */,
      0xD53A /* A3 + (-B3) * B0.  */ };

VECT_VAR_DECL (expected_fms1_static, hfloat, 16, 4) []
  = { 0x5C0D /* A0 + (-B0) * B1.  */,
      0xD0EE /* A1 + (-B1) * B1.  */,
      0x5274 /* A2 + (-B2) * B1.  */,
      0x5026 /* A3 + (-B3) * B1.  */ };

VECT_VAR_DECL (expected_fms2_static, hfloat, 16, 4) []
  = { 0xD54E /* A0 + (-B0) * B2.  */,
      0x51BA /* A1 + (-B1) * B2.  */,
      0xD4F3 /* A2 + (-B2) * B2.  */,
      0xCE66 /* A3 + (-B3) * B2.  */ };

VECT_VAR_DECL (expected_fms3_static, hfloat, 16, 4) []
  = { 0x4F70 /* A0 + (-B0) * B3.  */,
      0x4C5A /* A1 + (-B1) * B3.  */,
      0xD073 /* A2 + (-B2) * B3.  */,
      0xC600 /* A3 + (-B3) * B3.  */ };

VECT_VAR_DECL (expected_fms0_static, hfloat, 16, 8) []
  = { 0xDEA2 /* A0 + (-B0) * B0.  */,
      0x5810 /* A1 + (-B1) * B0.  */,
      0xDA82 /* A2 + (-B2) * B0.  */,
      0xD53A /* A3 + (-B3) * B0.  */,
      0x7C00 /* A4 + (-B4) * B0.  */,
      0x724B /* A5 + (-B5) * B0.  */,
      0x6286 /* A6 + (-B6) * B0.  */,
      0xFC00 /* A7 + (-B7) * B0.  */ };

VECT_VAR_DECL (expected_fms1_static, hfloat, 16, 8) []
  = { 0x5C0D /* A0 + (-B0) * B1.  */,
      0xD0EE /* A1 + (-B1) * B1.  */,
      0x5274 /* A2 + (-B2) * B1.  */,
      0x5026 /* A3 + (-B3) * B1.  */,
      0x7C00 /* A4 + (-B4) * B1.  */,
      0xEA41 /* A5 + (-B5) * B1.  */,
      0xD5DA /* A6 + (-B6) * B1.  */,
      0x7C00 /* A7 + (-B7) * B1.  */ };

VECT_VAR_DECL (expected_fms2_static, hfloat, 16, 8) []
  = { 0xD54E /* A0 + (-B0) * B2.  */,
      0x51BA /* A1 + (-B1) * B2.  */,
      0xD4F3 /* A2 + (-B2) * B2.  */,
      0xCE66 /* A3 + (-B3) * B2.  */,
      0x7C00 /* A4 + (-B4) * B2.  */,
      0x6CC8 /* A5 + (-B5) * B2.  */,
      0x5DD7 /* A6 + (-B6) * B2.  */,
      0xFC00 /* A7 + (-B7) * B2.  */ };

VECT_VAR_DECL (expected_fms3_static, hfloat, 16, 8) []
  = { 0x4F70 /* A0 + (-B0) * B3.  */,
      0x4C5A /* A1 + (-B1) * B3.  */,
      0xD073 /* A2 + (-B2) * B3.  */,
      0xC600 /* A3 + (-B3) * B3.  */,
      0x7C00 /* A4 + (-B4) * B3.  */,
      0x684B /* A5 + (-B5) * B3.  */,
      0x5AD0 /* A6 + (-B6) * B3.  */,
      0xFC00 /* A7 + (-B7) * B3.  */ };

VECT_VAR_DECL (expected_fms4_static, hfloat, 16, 8) []
  = { 0x5179 /* A0 + (-B0) * B4.  */,
      0x4AF6 /* A1 + (-B1) * B4.  */,
      0xCF91 /* A2 + (-B2) * B4.  */,
      0xC334 /* A3 + (-B3) * B4.  */,
      0x7C00 /* A4 + (-B4) * B4.  */,
      0x674C /* A5 + (-B5) * B4.  */,
      0x5A37 /* A6 + (-B6) * B4.  */,
      0xFC00 /* A7 + (-B7) * B4.  */ };

VECT_VAR_DECL (expected_fms5_static, hfloat, 16, 8) []
  = { 0x725C /* A0 + (-B0) * B5.  */,
      0xEA41 /* A1 + (-B1) * B5.  */,
      0x6CCA /* A2 + (-B2) * B5.  */,
      0x6853 /* A3 + (-B3) * B5.  */,
      0x7C00 /* A4 + (-B4) * B5.  */,
      0xFC00 /* A5 + (-B5) * B5.  */,
      0xF441 /* A6 + (-B6) * B5.  */,
      0x7C00 /* A7 + (-B7) * B5.  */ };

VECT_VAR_DECL (expected_fms6_static, hfloat, 16, 8) []
  = { 0x62C7 /* A0 + (-B0) * B6.  */,
      0xD9F2 /* A1 + (-B1) * B6.  */,
      0x5C6C /* A2 + (-B2) * B6.  */,
      0x584A /* A3 + (-B3) * B6.  */,
      0x7C00 /* A4 + (-B4) * B6.  */,
      0xF447 /* A5 + (-B5) * B6.  */,
      0xE330 /* A6 + (-B6) * B6.  */,
      0x7C00 /* A7 + (-B7) * B6.  */ };

VECT_VAR_DECL (expected_fms7_static, hfloat, 16, 8) []
  = { 0xFC00 /* A0 + (-B0) * B7.  */,
      0x7C00 /* A1 + (-B1) * B7.  */,
      0xFC00 /* A2 + (-B2) * B7.  */,
      0xFC00 /* A3 + (-B3) * B7.  */,
      0x7C00 /* A4 + (-B4) * B7.  */,
      0x7C00 /* A5 + (-B5) * B7.  */,
      0x7C00 /* A6 + (-B6) * B7.  */,
      0xFC00 /* A7 + (-B7) * B7.  */ };

void exec_vfmas_n_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VFMA_N (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 4);
  DECL_VARIABLE(vsrc_2, float, 16, 4);
  VECT_VAR_DECL (buf_src_1, float, 16, 4) [] = {A0, A1, A2, A3};
  VECT_VAR_DECL (buf_src_2, float, 16, 4) [] = {B0, B1, B2, B3};
  VLOAD (vsrc_1, buf_src_1, , float, f, 16, 4);
  VLOAD (vsrc_2, buf_src_2, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vfma_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B0);

  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fma0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vfma_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B1);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fma1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vfma_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B2);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fma2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vfma_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B3);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fma3_static, "");

#undef TEST_MSG
#define TEST_MSG "VFMAQ_N (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 8);
  DECL_VARIABLE(vsrc_2, float, 16, 8);
  VECT_VAR_DECL (buf_src_1, float, 16, 8) [] = {A0, A1, A2, A3, A4, A5, A6, A7};
  VECT_VAR_DECL (buf_src_2, float, 16, 8) [] = {B0, B1, B2, B3, B4, B5, B6, B7};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 16, 8);
  VLOAD (vsrc_2, buf_src_2, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B0);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B1);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B2);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B3);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma3_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B4);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma4_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B5);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma5_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B6);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma6_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmaq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B7);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fma7_static, "");

#undef TEST_MSG
#define TEST_MSG "VFMA_N (FP16)"
  clean_results ();

  VECT_VAR (vector_res, float, 16, 4)
    = vfms_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B0);

  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fms0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vfms_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B1);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fms1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vfms_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B2);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fms2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vfms_n_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		  VECT_VAR (vsrc_2, float, 16, 4), B3);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_fms3_static, "");

#undef TEST_MSG
#define TEST_MSG "VFMAQ_N (FP16)"
  clean_results ();

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B0);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B1);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B2);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B3);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms3_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B4);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms4_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B5);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms5_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B6);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms6_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vfmsq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		   VECT_VAR (vsrc_2, float, 16, 8), B7);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_fms7_static, "");
}

int
main (void)
{
  exec_vfmas_n_f16 ();
  return 0;
}
