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
#define E FP16_C (63.1)
#define F FP16_C (0.0)
#define G FP16_C (-4.8)
#define H FP16_C (0.0)

#define I FP16_C (0.7)
#define J FP16_C (-__builtin_inff ())
#define K FP16_C (11.23)
#define L FP16_C (98)
#define M FP16_C (87.1)
#define N FP16_C (-0.0)
#define O FP16_C (-1.1)
#define P FP16_C (7)

/* Expected results for vmulx.  */
VECT_VAR_DECL (expected_static, hfloat, 16, 4) []
  = { 0x629B /* A * E.  */, 0x4000 /* FP16_C (2.0f).  */,
      0x5939 /* C * G.  */, 0xC000 /* FP16_C (-2.0f).  */ };

VECT_VAR_DECL (expected_static, hfloat, 16, 8) []
  = { 0x629B /* A * E.  */, 0x4000 /* FP16_C (2.0f).  */,
      0x5939 /* C * G.  */, 0xC000 /* FP16_C (-2.0f).  */,
      0x53A0 /* I * M.  */, 0x4000 /* FP16_C (2.0f).  */,
      0xCA2C /* K * O.  */, 0x615C /* L * P.  */ };

void exec_vmulx_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VMULX (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 4);
  DECL_VARIABLE(vsrc_2, float, 16, 4);
  VECT_VAR_DECL (buf_src_1, float, 16, 4) [] = {A, B, C, D};
  VECT_VAR_DECL (buf_src_2, float, 16, 4) [] = {E, F, G, H};
  VLOAD (vsrc_1, buf_src_1, , float, f, 16, 4);
  VLOAD (vsrc_2, buf_src_2, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vmulx_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		 VECT_VAR (vsrc_2, float, 16, 4));
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_static, "");

#undef TEST_MSG
#define TEST_MSG "VMULXQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 8);
  DECL_VARIABLE(vsrc_2, float, 16, 8);
  VECT_VAR_DECL (buf_src_1, float, 16, 8) [] = {A, B, C, D, I, J, K, L};
  VECT_VAR_DECL (buf_src_2, float, 16, 8) [] = {E, F, G, H, M, N, O, P};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 16, 8);
  VLOAD (vsrc_2, buf_src_2, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vmulxq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		  VECT_VAR (vsrc_2, float, 16, 8));
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_static, "");
}

int
main (void)
{
  exec_vmulx_f16 ();
  return 0;
}
