/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A FP16_C (123.4)
#define RNDI_A 0x57B0 /* FP16_C (123).  */
#define B FP16_C (-567.5)
#define RNDI_B 0xE070 /* FP16_C (-568).  */
#define C FP16_C (-34.8)
#define RNDI_C 0xD060 /* FP16_C (-35).  */
#define D FP16_C (1024)
#define RNDI_D 0x6400 /* FP16_C (1024).  */
#define E FP16_C (663.1)
#define RNDI_E 0x612E /* FP16_C (663).  */
#define F FP16_C (169.1)
#define RNDI_F 0x5948 /* FP16_C (169).  */
#define G FP16_C (-4.8)
#define RNDI_G 0xC500 /* FP16_C (-5).  */
#define H FP16_C (77.5)
#define RNDI_H 0x54E0 /* FP16_C (78).  */

/* Expected results for vrndi.  */
VECT_VAR_DECL (expected_static, hfloat, 16, 4) []
  = { RNDI_A, RNDI_B, RNDI_C, RNDI_D };

VECT_VAR_DECL (expected_static, hfloat, 16, 8) []
  = { RNDI_A, RNDI_B, RNDI_C, RNDI_D, RNDI_E, RNDI_F, RNDI_G, RNDI_H };

void exec_vrndi_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VRNDI (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc, float, 16, 4);
  VECT_VAR_DECL (buf_src, float, 16, 4) [] = {A, B, C, D};
  VLOAD (vsrc, buf_src, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vrndi_f16 (VECT_VAR (vsrc, float, 16, 4));
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_static, "");

#undef TEST_MSG
#define TEST_MSG "VRNDIQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc, float, 16, 8);
  VECT_VAR_DECL (buf_src, float, 16, 8) [] = {A, B, C, D, E, F, G, H};
  VLOAD (vsrc, buf_src, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vrndiq_f16 (VECT_VAR (vsrc, float, 16, 8));
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_static, "");
}

int
main (void)
{
  exec_vrndi_f16 ();
  return 0;
}
