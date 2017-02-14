/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A FP16_C (123.4)
#define B FP16_C (__builtin_nanf ("")) /* NaN */
#define C FP16_C (-34.8)
#define D FP16_C (1024)
#define E FP16_C (663.1)
#define F FP16_C (169.1)
#define G FP16_C (-4.8)
#define H FP16_C (-__builtin_nanf ("")) /* NaN */

#define I FP16_C (0.7)
#define J FP16_C (-78)
#define K FP16_C (101.23)
#define L FP16_C (-1098)
#define M FP16_C (870.1)
#define N FP16_C (-8781)
#define O FP16_C (__builtin_inff ()) /* +Inf */
#define P FP16_C (-__builtin_inff ()) /* -Inf */


/* Expected results for vpminnm.  */
VECT_VAR_DECL (expected_min_static, hfloat, 16, 4) []
  = { 0x57B6 /* A.  */, 0xD05A /* C.  */, 0x5949 /* F.  */, 0xC4CD /* G.  */ };

VECT_VAR_DECL (expected_min_static, hfloat, 16, 8) []
  = { 0x57B6 /* A.  */, 0xD05A /* C.  */, 0xD4E0 /* J.  */, 0xE44A /* L.  */,
      0x5949 /* F.  */, 0xC4CD /* G.  */, 0xF04A /* N.  */, 0xFC00 /* P.  */ };

/* expected_max results for vpmaxnm.  */
VECT_VAR_DECL (expected_max_static, hfloat, 16, 4) []
  = { 0x57B6 /* A.  */, 0x6400 /* D.  */, 0x612E /* E.  */, 0xC4CD /* G.  */ };

VECT_VAR_DECL (expected_max_static, hfloat, 16, 8) []
  = { 0x57B6 /* A.  */, 0x6400 /* D.  */, 0x399A /* I.  */, 0x5654 /* K.  */,
      0x612E /* E.  */, 0xC4CD /* G.  */, 0x62CC /* M.  */, 0x7C00 /* O.  */ };

void exec_vpminmaxnm_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VPMINNM (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 4);
  DECL_VARIABLE(vsrc_2, float, 16, 4);
  VECT_VAR_DECL (buf_src_1, float, 16, 4) [] = {A, B, C, D};
  VECT_VAR_DECL (buf_src_2, float, 16, 4) [] = {E, F, G, H};
  VLOAD (vsrc_1, buf_src_1, , float, f, 16, 4);
  VLOAD (vsrc_2, buf_src_2, , float, f, 16, 4);
  DECL_VARIABLE (vector_res, float, 16, 4)
    = vpminnm_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		   VECT_VAR (vsrc_2, float, 16, 4));
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_min_static, "");

#undef TEST_MSG
#define TEST_MSG "VPMINNMQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc_1, float, 16, 8);
  DECL_VARIABLE(vsrc_2, float, 16, 8);
  VECT_VAR_DECL (buf_src_1, float, 16, 8) [] = {A, B, C, D, I, J, K, L};
  VECT_VAR_DECL (buf_src_2, float, 16, 8) [] = {E, F, G, H, M, N, O, P};
  VLOAD (vsrc_1, buf_src_1, q, float, f, 16, 8);
  VLOAD (vsrc_2, buf_src_2, q, float, f, 16, 8);
  DECL_VARIABLE (vector_res, float, 16, 8)
    = vpminnmq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		    VECT_VAR (vsrc_2, float, 16, 8));
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_min_static, "");

#undef TEST_MSG
#define TEST_MSG "VPMAXNM (FP16)"
  clean_results ();

  VECT_VAR (vector_res, float, 16, 4)
    = vpmaxnm_f16 (VECT_VAR (vsrc_1, float, 16, 4),
		   VECT_VAR (vsrc_2, float, 16, 4));
  vst1_f16 (VECT_VAR (result, float, 16, 4),
	    VECT_VAR (vector_res, float, 16, 4));

  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected_max_static, "");

#undef TEST_MSG
#define TEST_MSG "VPMAXNMQ (FP16)"
  clean_results ();

  VECT_VAR (vector_res, float, 16, 8)
    = vpmaxnmq_f16 (VECT_VAR (vsrc_1, float, 16, 8),
		    VECT_VAR (vsrc_2, float, 16, 8));
  vst1q_f16 (VECT_VAR (result, float, 16, 8),
	     VECT_VAR (vector_res, float, 16, 8));

  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected_max_static, "");
}

int
main (void)
{
  exec_vpminmaxnm_f16 ();
  return 0;
}
