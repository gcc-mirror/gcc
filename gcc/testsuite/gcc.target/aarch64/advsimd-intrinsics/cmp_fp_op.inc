/* Template file for the validation of comparison operator with
   floating-point support.

   This file is meant to be included by the relevant test files, which
   have to define the intrinsic family to test. If a given intrinsic
   supports variants which are not supported by all the other
   operators, these can be tested by providing a definition for
   EXTRA_TESTS.  */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Additional expected results declaration, they are initialized in
   each test file.  */
extern ARRAY(expected2, uint, 32, 2);
extern ARRAY(expected2, uint, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
extern ARRAY(expected2, uint, 16, 4);
extern ARRAY(expected2, uint, 16, 8);
#endif

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=vcomp(x1,x2), then store the result.  */
#define TEST_VCOMP1(INSN, Q, T1, T2, T3, W, N)				\
  VECT_VAR(vector_res, T3, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_u##W(VECT_VAR(result, T3, W, N), VECT_VAR(vector_res, T3, W, N))

#define TEST_VCOMP(INSN, Q, T1, T2, T3, W, N)				\
  TEST_VCOMP1(INSN, Q, T1, T2, T3, W, N)

  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, float, 32, 4);
  DECL_VARIABLE(vector2, float, 32, 2);
  DECL_VARIABLE(vector2, float, 32, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);
  DECL_VARIABLE(vector2, float, 16, 4);
  DECL_VARIABLE(vector2, float, 16, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 16, 8);
#endif

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif

  /* Choose init value arbitrarily, will be used for vector
     comparison.  */
  VDUP(vector2, , float, f, 32, 2, -16.0f);
  VDUP(vector2, q, float, f, 32, 4, -14.0f);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector2, , float, f, 16, 4, -16.0f);
  VDUP(vector2, q, float, f, 16, 8, -14.0f);
#endif

  /* Apply operator named INSN_NAME.  */
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");

  TEST_VCOMP(INSN_NAME, q, float, f, uint, 32, 4);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VCOMP(INSN_NAME, , float, f, uint, 16, 4);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");

  TEST_VCOMP(INSN_NAME, q, float, f, uint, 16, 8);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
#endif

  /* Test again, with different input values.  */
  VDUP(vector2, , float, f, 32, 2, -10.0f);
  VDUP(vector2, q, float, f, 32, 4, 10.0f);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector2, , float, f, 16, 4, -10.0f);
  VDUP(vector2, q, float, f, 16, 8, 10.0f);
#endif

  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected2, "");

  TEST_VCOMP(INSN_NAME, q, float, f, uint, 32, 4);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected2,"");

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VCOMP(INSN_NAME, , float, f, uint, 16, 4);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected2, "");

  TEST_VCOMP(INSN_NAME, q, float, f, uint, 16, 8);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected2,"");
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
