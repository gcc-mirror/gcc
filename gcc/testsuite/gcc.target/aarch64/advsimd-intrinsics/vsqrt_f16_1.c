/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A FP16_C (123.4)
#define B FP16_C (567.8)
#define C FP16_C (34.8)
#define D FP16_C (1024)
#define E FP16_C (663.1)
#define F FP16_C (144.0)
#define G FP16_C (4.8)
#define H FP16_C (77)

#define SQRT_A 0x498E /* FP16_C (__builtin_sqrtf (123.4)).  */
#define SQRT_B 0x4DF5 /* FP16_C (__builtin_sqrtf (567.8)).  */
#define SQRT_C 0x45E6 /* FP16_C (__builtin_sqrtf (34.8)).  */
#define SQRT_D 0x5000 /* FP16_C (__builtin_sqrtf (1024)).  */
#define SQRT_E 0x4E70 /* FP16_C (__builtin_sqrtf (663.1)).  */
#define SQRT_F 0x4A00 /* FP16_C (__builtin_sqrtf (144.0)).  */
#define SQRT_G 0x4062 /* FP16_C (__builtin_sqrtf (4.8)).  */
#define SQRT_H 0x4863 /* FP16_C (__builtin_sqrtf (77)).  */

/* Expected results for vsqrt.  */
VECT_VAR_DECL (expected_static, hfloat, 16, 4) []
  = { SQRT_A, SQRT_B, SQRT_C, SQRT_D };

VECT_VAR_DECL (expected_static, hfloat, 16, 8) []
  = { SQRT_A, SQRT_B, SQRT_C, SQRT_D, SQRT_E, SQRT_F, SQRT_G, SQRT_H };

void exec_vsqrt_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VSQRT (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc, float, 16, 4);
  VECT_VAR_DECL (buf_src, float, 16, 4) [] = {A, B, C, D};
  VLOAD (vsrc, buf_src, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vsqrt_f16 (VECT_VAR (vsrc, float, 16, 4));
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_static, "");

#undef TEST_MSG
#define TEST_MSG "VSQRTQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc, float, 16, 8);
  VECT_VAR_DECL (buf_src, float, 16, 8) [] = {A, B, C, D, E, F, G, H};
  VLOAD (vsrc, buf_src, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vsqrtq_f16 (VECT_VAR (vsrc, float, 16, 8));
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_static, "");
}

int
main (void)
{
  exec_vsqrt_f16 ();
  return 0;
}
