/* Template file for the validation of compare against zero operators.

   This file is base on cmp_op.inc.  It is meant to be included by the relevant
   test files, which have to define the intrinsic family to test.  If a given
   intrinsic supports variants which are not supported by all the other
   operators, these can be tested by providing a definition for EXTRA_TESTS.  */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Additional expected results declaration, they are initialized in
   each test file.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
extern ARRAY(expected_float, uint, 16, 4);
extern ARRAY(expected_q_float, uint, 16, 8);
extern ARRAY(expected_uint2, uint, 16, 4);
extern ARRAY(expected_uint3, uint, 16, 4);
extern ARRAY(expected_uint4, uint, 16, 4);
extern ARRAY(expected_nan, uint, 16, 4);
extern ARRAY(expected_mnan, uint, 16, 4);
extern ARRAY(expected_inf, uint, 16, 4);
extern ARRAY(expected_minf, uint, 16, 4);
extern ARRAY(expected_zero, uint, 16, 4);
extern ARRAY(expected_mzero, uint, 16, 4);
#endif

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=vcomp(x1,x2), then store the result.  */
#define TEST_VCOMP1(INSN, Q, T1, T2, T3, W, N)				\
  VECT_VAR(vector_res, T3, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
  vst1##Q##_u##W(VECT_VAR(result, T3, W, N), VECT_VAR(vector_res, T3, W, N))

#define TEST_VCOMP(INSN, Q, T1, T2, T3, W, N)				\
  TEST_VCOMP1(INSN, Q, T1, T2, T3, W, N)

  /* No need for 64 bits elements.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE (vector, float, 16, 4);
  DECL_VARIABLE (vector, float, 16, 8);
#endif

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 16, 8);
#endif

  clean_results ();

  /* Choose init value arbitrarily, will be used for vector
     comparison.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP (vector, , float, f, 16, 4, -15.0f);
  VDUP (vector, q, float, f, 16, 8, 14.0f);
#endif

  /* Float variants.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  TEST_VCOMP (INSN_NAME, q, float, f, uint, 16, 8);
#endif

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_float, "");
  CHECK (TEST_MSG, uint, 16, 8, PRIx16, expected_q_float, "");
#endif

  /* Extra FP tests with special values (NaN, ....).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP (vector, , float, f, 16, 4, NAN);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_nan, "FP special (NaN)");

  VDUP (vector, , float, f, 16, 4, -NAN);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_mnan, " FP special (-NaN)");

  VDUP (vector, , float, f, 16, 4, HUGE_VALF);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_inf, " FP special (inf)");

  VDUP (vector, , float, f, 16, 4, -HUGE_VALF);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_minf, " FP special (-inf)");

  VDUP (vector, , float, f, 16, 4, 0.0);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_zero, " FP special (0.0)");

  VDUP (vector, , float, f, 16, 4, 0.0);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_mzero, " FP special (-0.0)");
#endif

#ifdef EXTRA_TESTS
  EXTRA_TESTS();
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
