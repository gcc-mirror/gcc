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

float16_t elemE = E;
float16_t elemF = F;
float16_t elemG = G;
float16_t elemH = H;

#define I FP16_C (0.7)
#define J FP16_C (-78)
#define K FP16_C (11.23)
#define L FP16_C (98)
#define M FP16_C (87.1)
#define N FP16_C (-8)
#define O FP16_C (-1.1)
#define P FP16_C (-9.7)

/* Expected results for vmulx_n.  */
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

VECT_VAR_DECL (expected0_static, hfloat, 16, 8) []
  = { 0x8000 /* A * E.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* C * E.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* I * E.  */,
      0x0000 /* J * E.  */,
      0x8000 /* K * E.  */,
      0x8000 /* L * E.  */ };

VECT_VAR_DECL (expected1_static, hfloat, 16, 8) []
  = { 0x5BFF /* A * F.  */,
      0x7C00 /* B * F.  */,
      0xE131 /* C * F.  */,
      0xFC00 /* D * F.  */,
      0x4AAF /* I * F.  */,
      0xE5D1 /* J * F.  */,
      0x5AB3 /* K * F.  */,
      0x674F /* L * F.  */ };

VECT_VAR_DECL (expected2_static, hfloat, 16, 8) []
  = { 0xD405 /* A * G.  */,
      0xFC00 /* B * G.  */,
      0x5939 /* C * G.  */,
      0x7C00 /* D * G.  */,
      0xC2B9 /* I * G.  */,
      0x5DDA /* J * G.  */,
      0xD2BD /* K * G.  */,
      0xDF5A /* L * G.  */ };

VECT_VAR_DECL (expected3_static, hfloat, 16, 8) []
  = { 0x0000 /* A * H.  */,
      0x4000 /* FP16_C (2.0f).  */,
      0x8000 /* C * H.  */,
      0xC000 /* FP16_C (-2.0f).  */,
      0x0000 /* I * H.  */,
      0x8000 /* J * H.  */,
      0x0000 /* K * H.  */,
      0x0000 /* L * H.  */ };

void exec_vmulx_n_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VMULX_N (FP16)"
  clean_results ();

  DECL_VARIABLE (vsrc_1, float, 16, 4);
  VECT_VAR_DECL (buf_src_1, float, 16, 4) [] = {A, B, C, D};
  VLOAD (vsrc_1, buf_src_1, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vmulx_n_f16 (VECT_VAR (vsrc_1, float, 16, 4), elemE);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected0_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_n_f16 (VECT_VAR (vsrc_1, float, 16, 4), elemF);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected1_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_n_f16 (VECT_VAR (vsrc_1, float, 16, 4), elemG);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected2_static, "");

  VECT_VAR (vector_res, float, 16, 4)
    = vmulx_n_f16 (VECT_VAR (vsrc_1, float, 16, 4), elemH);
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected3_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULXQ_N (FP16)"
  clean_results ();

  DECL_VARIABLE (vsrc_1, float, 16, 8);
  VECT_VAR_DECL (buf_src_1, float, 16, 8) [] = {A, B, C, D, I, J, K, L};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vmulxq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8), elemE);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected0_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8), elemF);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected1_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8), elemG);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected2_static, "");

  VECT_VAR (vector_res, float, 16, 8)
    = vmulxq_n_f16 (VECT_VAR (vsrc_1, float, 16, 8), elemH);
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected3_static, "");
}

int
main (void)
{
  exec_vmulx_n_f16 ();
  return 0;
}
