#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#if defined(__aarch64__) && defined(__ARM_FEATURE_FMA)

#define A0 123.4f
#define A1 -3.8f
#define A2 -29.4f
#define A3 (__builtin_inff ())
#define A4 0.0f
#define A5 24.0f
#define A6 124.0f
#define A7 1024.0f

#define B0 -5.8f
#define B1 -0.0f
#define B2 -10.8f
#define B3 10.0f
#define B4 23.4f
#define B5 -1234.8f
#define B6 8.9f
#define B7 4.0f

#define E0 9.8f
#define E1 -1024.0f
#define E2 (-__builtin_inff ())
#define E3 479.0f
float32_t elem0 = E0;
float32_t elem1 = E1;
float32_t elem2 = E2;
float32_t elem3 = E3;

#define DA0 1231234.4
#define DA1 -3.8
#define DA2 -2980.4
#define DA3 -5.8
#define DA4 0.01123
#define DA5 24.0
#define DA6 124.12345
#define DA7 1024.0

#define DB0 -5.8
#define DB1 (__builtin_inf ())
#define DB2 -105.8
#define DB3 10.0
#define DB4 (-__builtin_inf ())
#define DB5 -1234.8
#define DB6 848.9
#define DB7 44444.0

#define DE0 9.8
#define DE1 -1024.0
#define DE2 105.8
#define DE3 479.0
float64_t delem0 = DE0;
float64_t delem1 = DE1;
float64_t delem2 = DE2;
float64_t delem3 = DE3;

/* Expected results for vfms_n.  */

VECT_VAR_DECL(expectedfms0, float, 32, 2) [] = {A0 + -B0 * E0, A1 + -B1 * E0};
VECT_VAR_DECL(expectedfms1, float, 32, 2) [] = {A2 + -B2 * E1, A3 + -B3 * E1};
VECT_VAR_DECL(expectedfms2, float, 32, 2) [] = {A4 + -B4 * E2, A5 + -B5 * E2};
VECT_VAR_DECL(expectedfms3, float, 32, 2) [] = {A6 + -B6 * E3, A7 + -B7 * E3};
VECT_VAR_DECL(expectedfma0, float, 32, 2) [] = {A0 + B0 * E0, A1 + B1 * E0};
VECT_VAR_DECL(expectedfma1, float, 32, 2) [] = {A2 + B2 * E1, A3 + B3 * E1};
VECT_VAR_DECL(expectedfma2, float, 32, 2) [] = {A4 + B4 * E2, A5 + B5 * E2};
VECT_VAR_DECL(expectedfma3, float, 32, 2) [] = {A6 + B6 * E3, A7 + B7 * E3};

hfloat32_t * VECT_VAR (expectedfms0_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfms0, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfms1_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfms1, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfms2_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfms2, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfms3_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfms3, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfma0_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfma0, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfma1_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfma1, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfma2_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfma2, float, 32, 2);
hfloat32_t * VECT_VAR (expectedfma3_static, hfloat, 32, 2) =
  (hfloat32_t *) VECT_VAR (expectedfma3, float, 32, 2);


VECT_VAR_DECL(expectedfms0, float, 32, 4) [] = {A0 + -B0 * E0, A1 + -B1 * E0,
						A2 + -B2 * E0, A3 + -B3 * E0};
VECT_VAR_DECL(expectedfms1, float, 32, 4) [] = {A4 + -B4 * E1, A5 + -B5 * E1,
						A6 + -B6 * E1, A7 + -B7 * E1};
VECT_VAR_DECL(expectedfms2, float, 32, 4) [] = {A0 + -B0 * E2, A2 + -B2 * E2,
						A4 + -B4 * E2, A6 + -B6 * E2};
VECT_VAR_DECL(expectedfms3, float, 32, 4) [] = {A1 + -B1 * E3, A3 + -B3 * E3,
						A5 + -B5 * E3, A7 + -B7 * E3};
VECT_VAR_DECL(expectedfma0, float, 32, 4) [] = {A0 + B0 * E0, A1 + B1 * E0,
						A2 + B2 * E0, A3 + B3 * E0};
VECT_VAR_DECL(expectedfma1, float, 32, 4) [] = {A4 + B4 * E1, A5 + B5 * E1,
						A6 + B6 * E1, A7 + B7 * E1};
VECT_VAR_DECL(expectedfma2, float, 32, 4) [] = {A0 + B0 * E2, A2 + B2 * E2,
						A4 + B4 * E2, A6 + B6 * E2};
VECT_VAR_DECL(expectedfma3, float, 32, 4) [] = {A1 + B1 * E3, A3 + B3 * E3,
						A5 + B5 * E3, A7 + B7 * E3};

hfloat32_t * VECT_VAR (expectedfms0_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfms0, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfms1_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfms1, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfms2_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfms2, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfms3_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfms3, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfma0_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfma0, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfma1_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfma1, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfma2_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfma2, float, 32, 4);
hfloat32_t * VECT_VAR (expectedfma3_static, hfloat, 32, 4) =
  (hfloat32_t *) VECT_VAR (expectedfma3, float, 32, 4);

VECT_VAR_DECL(expectedfms0, float, 64, 2) [] = {DA0 + -DB0 * DE0,
						DA1 + -DB1 * DE0};
VECT_VAR_DECL(expectedfms1, float, 64, 2) [] = {DA2 + -DB2 * DE1,
						DA3 + -DB3 * DE1};
VECT_VAR_DECL(expectedfms2, float, 64, 2) [] = {DA4 + -DB4 * DE2,
						DA5 + -DB5 * DE2};
VECT_VAR_DECL(expectedfms3, float, 64, 2) [] = {DA6 + -DB6 * DE3,
						DA7 + -DB7 * DE3};
VECT_VAR_DECL(expectedfma0, float, 64, 2) [] = {DA0 + DB0 * DE0,
						DA1 + DB1 * DE0};
VECT_VAR_DECL(expectedfma1, float, 64, 2) [] = {DA2 + DB2 * DE1,
						DA3 + DB3 * DE1};
VECT_VAR_DECL(expectedfma2, float, 64, 2) [] = {DA4 + DB4 * DE2,
						DA5 + DB5 * DE2};
VECT_VAR_DECL(expectedfma3, float, 64, 2) [] = {DA6 + DB6 * DE3,
						DA7 + DB7 * DE3};
hfloat64_t * VECT_VAR (expectedfms0_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfms0, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfms1_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfms1, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfms2_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfms2, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfms3_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfms3, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfma0_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfma0, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfma1_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfma1, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfma2_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfma2, float, 64, 2);
hfloat64_t * VECT_VAR (expectedfma3_static, hfloat, 64, 2) =
  (hfloat64_t *) VECT_VAR (expectedfma3, float, 64, 2);

VECT_VAR_DECL(expectedfms0, float, 64, 1) [] = {DA0 + -DB0 * DE0};
VECT_VAR_DECL(expectedfms1, float, 64, 1) [] = {DA2 + -DB2 * DE1};
VECT_VAR_DECL(expectedfms2, float, 64, 1) [] = {DA4 + -DB4 * DE2};
VECT_VAR_DECL(expectedfms3, float, 64, 1) [] = {DA6 + -DB6 * DE3};
VECT_VAR_DECL(expectedfma0, float, 64, 1) [] = {DA0 + DB0 * DE0};
VECT_VAR_DECL(expectedfma1, float, 64, 1) [] = {DA2 + DB2 * DE1};
VECT_VAR_DECL(expectedfma2, float, 64, 1) [] = {DA4 + DB4 * DE2};
VECT_VAR_DECL(expectedfma3, float, 64, 1) [] = {DA6 + DB6 * DE3};

hfloat64_t * VECT_VAR (expectedfms0_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfms0, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfms1_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfms1, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfms2_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfms2, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfms3_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfms3, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfma0_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfma0, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfma1_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfma1, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfma2_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfma2, float, 64, 1);
hfloat64_t * VECT_VAR (expectedfma3_static, hfloat, 64, 1) =
  (hfloat64_t *) VECT_VAR (expectedfma3, float, 64, 1);

void exec_vfma_vfms_n (void)
{
#undef TEST_MSG
#define TEST_MSG "VFMS_VFMA_N (FP32)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 32, 2);
  DECL_VARIABLE(vsrc_2, float, 32, 2);
  VECT_VAR_DECL (buf_src_1, float, 32, 2) [] = {A0, A1};
  VECT_VAR_DECL (buf_src_2, float, 32, 2) [] = {B0, B1};
  VLOAD (vsrc_1, buf_src_1, , float, f, 32, 2);
  VLOAD (vsrc_2, buf_src_2, , float, f, 32, 2);
  DECL_VARIABLE (vector_res, float, 32, 2) =
    vfms_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem0);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfms0_static, "");
  VECT_VAR (vector_res, float, 32, 2) =
    vfma_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem0);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfma0_static, "");

  VECT_VAR_DECL (buf_src_3, float, 32, 2) [] = {A2, A3};
  VECT_VAR_DECL (buf_src_4, float, 32, 2) [] = {B2, B3};
  VLOAD (vsrc_1, buf_src_3, , float, f, 32, 2);
  VLOAD (vsrc_2, buf_src_4, , float, f, 32, 2);
  VECT_VAR (vector_res, float, 32, 2) =
    vfms_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem1);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfms1_static, "");
  VECT_VAR (vector_res, float, 32, 2) =
    vfma_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem1);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfma1_static, "");

  VECT_VAR_DECL (buf_src_5, float, 32, 2) [] = {A4, A5};
  VECT_VAR_DECL (buf_src_6, float, 32, 2) [] = {B4, B5};
  VLOAD (vsrc_1, buf_src_5, , float, f, 32, 2);
  VLOAD (vsrc_2, buf_src_6, , float, f, 32, 2);
  VECT_VAR (vector_res, float, 32, 2) =
    vfms_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem2);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfms2_static, "");
  VECT_VAR (vector_res, float, 32, 2) =
    vfma_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem2);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfma2_static, "");

  VECT_VAR_DECL (buf_src_7, float, 32, 2) [] = {A6, A7};
  VECT_VAR_DECL (buf_src_8, float, 32, 2) [] = {B6, B7};
  VLOAD (vsrc_1, buf_src_7, , float, f, 32, 2);
  VLOAD (vsrc_2, buf_src_8, , float, f, 32, 2);
  VECT_VAR (vector_res, float, 32, 2) =
    vfms_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem3);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfms3_static, "");
  VECT_VAR (vector_res, float, 32, 2) =
    vfma_n_f32 (VECT_VAR (vsrc_1, float, 32, 2),
		VECT_VAR (vsrc_2, float, 32, 2), elem3);
  vst1_f32 (VECT_VAR (result, float, 32, 2),
	    VECT_VAR (vector_res, float, 32, 2));
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx16, expectedfma3_static, "");

#undef TEST_MSG
#define TEST_MSG "VFMSQ_VFMAQ_N (FP32)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 32, 4);
  DECL_VARIABLE(vsrc_2, float, 32, 4);
  VECT_VAR_DECL (buf_src_1, float, 32, 4) [] = {A0, A1, A2, A3};
  VECT_VAR_DECL (buf_src_2, float, 32, 4) [] = {B0, B1, B2, B3};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 32, 4);
  VLOAD (vsrc_2, buf_src_2, q, float, f, 32, 4);
  DECL_VARIABLE (vector_res, float, 32, 4) =
    vfmsq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem0);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfms0_static, "");
  VECT_VAR (vector_res, float, 32, 4) =
    vfmaq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem0);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfma0_static, "");

  VECT_VAR_DECL (buf_src_3, float, 32, 4) [] = {A4, A5, A6, A7};
  VECT_VAR_DECL (buf_src_4, float, 32, 4) [] = {B4, B5, B6, B7};
  VLOAD (vsrc_1, buf_src_3, q, float, f, 32, 4);
  VLOAD (vsrc_2, buf_src_4, q, float, f, 32, 4);
  VECT_VAR (vector_res, float, 32, 4) =
    vfmsq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem1);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfms1_static, "");
  VECT_VAR (vector_res, float, 32, 4) =
    vfmaq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem1);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfma1_static, "");

  VECT_VAR_DECL (buf_src_5, float, 32, 4) [] = {A0, A2, A4, A6};
  VECT_VAR_DECL (buf_src_6, float, 32, 4) [] = {B0, B2, B4, B6};
  VLOAD (vsrc_1, buf_src_5, q, float, f, 32, 4);
  VLOAD (vsrc_2, buf_src_6, q, float, f, 32, 4);
  VECT_VAR (vector_res, float, 32, 4) =
    vfmsq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem2);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfms2_static, "");
  VECT_VAR (vector_res, float, 32, 4) =
    vfmaq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem2);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfma2_static, "");

  VECT_VAR_DECL (buf_src_7, float, 32, 4) [] = {A1, A3, A5, A7};
  VECT_VAR_DECL (buf_src_8, float, 32, 4) [] = {B1, B3, B5, B7};
  VLOAD (vsrc_1, buf_src_7, q, float, f, 32, 4);
  VLOAD (vsrc_2, buf_src_8, q, float, f, 32, 4);
  VECT_VAR (vector_res, float, 32, 4) =
    vfmsq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem3);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfms3_static, "");
  VECT_VAR (vector_res, float, 32, 4) =
    vfmaq_n_f32 (VECT_VAR (vsrc_1, float, 32, 4),
		 VECT_VAR (vsrc_2, float, 32, 4), elem3);
  vst1q_f32 (VECT_VAR (result, float, 32, 4),
	     VECT_VAR (vector_res, float, 32, 4));
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx16, expectedfma3_static, "");

#undef TEST_MSG
#define TEST_MSG "VFMSQ_VFMAQ_N (FP64)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 64, 2);
  DECL_VARIABLE(vsrc_2, float, 64, 2);
  VECT_VAR_DECL (buf_src_1, float, 64, 2) [] = {DA0, DA1};
  VECT_VAR_DECL (buf_src_2, float, 64, 2) [] = {DB0, DB1};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 64, 2);
  VLOAD (vsrc_2, buf_src_2, q, float, f, 64, 2);
  DECL_VARIABLE (vector_res, float, 64, 2) =
    vfmsq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem0);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfms0_static, "");
  VECT_VAR (vector_res, float, 64, 2) =
    vfmaq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem0);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfma0_static, "");

  VECT_VAR_DECL (buf_src_3, float, 64, 2) [] = {DA2, DA3};
  VECT_VAR_DECL (buf_src_4, float, 64, 2) [] = {DB2, DB3};
  VLOAD (vsrc_1, buf_src_3, q, float, f, 64, 2);
  VLOAD (vsrc_2, buf_src_4, q, float, f, 64, 2);
  VECT_VAR (vector_res, float, 64, 2) =
    vfmsq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem1);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfms1_static, "");
  VECT_VAR (vector_res, float, 64, 2) =
    vfmaq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem1);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfma1_static, "");

  VECT_VAR_DECL (buf_src_5, float, 64, 2) [] = {DA4, DA5};
  VECT_VAR_DECL (buf_src_6, float, 64, 2) [] = {DB4, DB5};
  VLOAD (vsrc_1, buf_src_5, q, float, f, 64, 2);
  VLOAD (vsrc_2, buf_src_6, q, float, f, 64, 2);
  VECT_VAR (vector_res, float, 64, 2) =
    vfmsq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem2);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfms2_static, "");
  VECT_VAR (vector_res, float, 64, 2) =
    vfmaq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem2);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfma2_static, "");

  VECT_VAR_DECL (buf_src_7, float, 64, 2) [] = {DA6, DA7};
  VECT_VAR_DECL (buf_src_8, float, 64, 2) [] = {DB6, DB7};
  VLOAD (vsrc_1, buf_src_7, q, float, f, 64, 2);
  VLOAD (vsrc_2, buf_src_8, q, float, f, 64, 2);
  VECT_VAR (vector_res, float, 64, 2) =
    vfmsq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem3);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfms3_static, "");
  VECT_VAR (vector_res, float, 64, 2) =
    vfmaq_n_f64 (VECT_VAR (vsrc_1, float, 64, 2),
		 VECT_VAR (vsrc_2, float, 64, 2), delem3);
  vst1q_f64 (VECT_VAR (result, float, 64, 2),
	     VECT_VAR (vector_res, float, 64, 2));
  CHECK_FP (TEST_MSG, float, 64, 2, PRIx16, expectedfma3_static, "");

#undef TEST_MSG
#define TEST_MSG "VFMS_VFMA_N (FP64)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 64, 1);
  DECL_VARIABLE(vsrc_2, float, 64, 1);
  VECT_VAR_DECL (buf_src_1, float, 64, 1) [] = {DA0};
  VECT_VAR_DECL (buf_src_2, float, 64, 1) [] = {DB0};
  VLOAD (vsrc_1, buf_src_1, , float, f, 64, 1);
  VLOAD (vsrc_2, buf_src_2, , float, f, 64, 1);
  DECL_VARIABLE (vector_res, float, 64, 1) =
    vfms_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem0);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfms0_static, "");
  VECT_VAR (vector_res, float, 64, 1) =
    vfma_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem0);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfma0_static, "");

  VECT_VAR_DECL (buf_src_3, float, 64, 1) [] = {DA2};
  VECT_VAR_DECL (buf_src_4, float, 64, 1) [] = {DB2};
  VLOAD (vsrc_1, buf_src_3, , float, f, 64, 1);
  VLOAD (vsrc_2, buf_src_4, , float, f, 64, 1);
  VECT_VAR (vector_res, float, 64, 1) =
    vfms_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem1);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfms1_static, "");
  VECT_VAR (vector_res, float, 64, 1) =
    vfma_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem1);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfma1_static, "");

  VECT_VAR_DECL (buf_src_5, float, 64, 1) [] = {DA4};
  VECT_VAR_DECL (buf_src_6, float, 64, 1) [] = {DB4};
  VLOAD (vsrc_1, buf_src_5, , float, f, 64, 1);
  VLOAD (vsrc_2, buf_src_6, , float, f, 64, 1);
  VECT_VAR (vector_res, float, 64, 1) =
    vfms_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem2);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfms2_static, "");
  VECT_VAR (vector_res, float, 64, 1) =
    vfma_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem2);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfma2_static, "");

  VECT_VAR_DECL (buf_src_7, float, 64, 1) [] = {DA6};
  VECT_VAR_DECL (buf_src_8, float, 64, 1) [] = {DB6};
  VLOAD (vsrc_1, buf_src_7, , float, f, 64, 1);
  VLOAD (vsrc_2, buf_src_8, , float, f, 64, 1);
  VECT_VAR (vector_res, float, 64, 1) =
    vfms_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem3);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfms3_static, "");
  VECT_VAR (vector_res, float, 64, 1) =
    vfma_n_f64 (VECT_VAR (vsrc_1, float, 64, 1),
		VECT_VAR (vsrc_2, float, 64, 1), delem3);
  vst1_f64 (VECT_VAR (result, float, 64, 1),
	     VECT_VAR (vector_res, float, 64, 1));
  CHECK_FP (TEST_MSG, float, 64, 1, PRIx16, expectedfma3_static, "");
}
#endif

int
main (void)
{
#if defined(__aarch64__) && defined(__ARM_FEATURE_FMA)
  exec_vfma_vfms_n ();
#endif
  return 0;
}
