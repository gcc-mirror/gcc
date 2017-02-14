#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Additional expected results declaration, they are initialized in
   each test file.  */
extern ARRAY(expected_uint, uint, 8, 8);
extern ARRAY(expected_uint, uint, 16, 4);
extern ARRAY(expected_uint, uint, 32, 2);
extern ARRAY(expected_q_uint, uint, 8, 16);
extern ARRAY(expected_q_uint, uint, 16, 8);
extern ARRAY(expected_q_uint, uint, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
extern ARRAY(expected_float, uint, 16, 4);
extern ARRAY(expected_q_float, uint, 16, 8);
extern ARRAY(expected_nan, uint, 16, 4);
extern ARRAY(expected_mnan, uint, 16, 4);
extern ARRAY(expected_nan2, uint, 16, 4);
extern ARRAY(expected_inf, uint, 16, 4);
extern ARRAY(expected_minf, uint, 16, 4);
extern ARRAY(expected_inf2, uint, 16, 4);
extern ARRAY(expected_mzero, uint, 16, 4);
#endif
extern ARRAY(expected_float, uint, 32, 2);
extern ARRAY(expected_q_float, uint, 32, 4);
extern ARRAY(expected_uint2, uint, 32, 2);
extern ARRAY(expected_uint3, uint, 32, 2);
extern ARRAY(expected_uint4, uint, 32, 2);
extern ARRAY(expected_nan, uint, 32, 2);
extern ARRAY(expected_mnan, uint, 32, 2);
extern ARRAY(expected_nan2, uint, 32, 2);
extern ARRAY(expected_inf, uint, 32, 2);
extern ARRAY(expected_minf, uint, 32, 2);
extern ARRAY(expected_inf2, uint, 32, 2);
extern ARRAY(expected_mzero, uint, 32, 2);
extern ARRAY(expected_p8, uint, 8, 8);
extern ARRAY(expected_q_p8, uint, 8, 16);

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

  /* No need for 64 bits elements.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE (vector, float, 16, 4);
#endif
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE (vector, float, 16, 8);
#endif
  DECL_VARIABLE(vector, float, 32, 4);

  DECL_VARIABLE(vector2, int, 8, 8);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, uint, 8, 8);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector2, uint, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE (vector2, float, 16, 4);
#endif
  DECL_VARIABLE(vector2, float, 32, 2);
  DECL_VARIABLE(vector2, int, 8, 16);
  DECL_VARIABLE(vector2, int, 16, 8);
  DECL_VARIABLE(vector2, int, 32, 4);
  DECL_VARIABLE(vector2, uint, 8, 16);
  DECL_VARIABLE(vector2, uint, 16, 8);
  DECL_VARIABLE(vector2, uint, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE (vector2, float, 16, 8);
#endif
  DECL_VARIABLE(vector2, float, 32, 4);

  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  clean_results ();

  /* There is no 64 bits variant, don't use the generic initializer.  */
  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 8, 8);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD (vector, buffer, , float, f, 16, 4);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);

  VLOAD(vector, buffer, q, int, s, 8, 16);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 8, 16);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD (vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Choose init value arbitrarily, will be used for vector
     comparison.  */
  VDUP(vector2, , int, s, 8, 8, -10);
  VDUP(vector2, , int, s, 16, 4, -14);
  VDUP(vector2, , int, s, 32, 2, -16);
  VDUP(vector2, , uint, u, 8, 8, 0xF3);
  VDUP(vector2, , uint, u, 16, 4, 0xFFF2);
  VDUP(vector2, , uint, u, 32, 2, 0xFFFFFFF1);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP (vector2, , float, f, 16, 4, -15.0f);
#endif
  VDUP(vector2, , float, f, 32, 2, -15.0f);

  VDUP(vector2, q, int, s, 8, 16, -4);
  VDUP(vector2, q, int, s, 16, 8, -10);
  VDUP(vector2, q, int, s, 32, 4, -14);
  VDUP(vector2, q, uint, u, 8, 16, 0xF4);
  VDUP(vector2, q, uint, u, 16, 8, 0xFFF6);
  VDUP(vector2, q, uint, u, 32, 4, 0xFFFFFFF2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP (vector2, q, float, f, 16, 8, -14.0f);
#endif
  VDUP(vector2, q, float, f, 32, 4, -14.0f);

  /* The comparison operators produce only unsigned results, which
     means that our tests with uint* inputs write their results in the
     same vectors as the int* variants. As a consequence, we have to
     execute and test the int* first, then the uint* ones.
     Same thing for float and poly8.
  */

  /* Apply operator named INSN_NAME.  */
  TEST_VCOMP(INSN_NAME, , int, s, uint, 8, 8);
  TEST_VCOMP(INSN_NAME, , int, s, uint, 16, 4);
  TEST_VCOMP(INSN_NAME, , int, s, uint, 32, 2);
  TEST_VCOMP(INSN_NAME, q, int, s, uint, 8, 16);
  TEST_VCOMP(INSN_NAME, q, int, s, uint, 16, 8);
  TEST_VCOMP(INSN_NAME, q, int, s, uint, 32, 4);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");

  /* Now the uint* variants.  */
  TEST_VCOMP(INSN_NAME, , uint, u, uint, 8, 8);
  TEST_VCOMP(INSN_NAME, , uint, u, uint, 16, 4);
  TEST_VCOMP(INSN_NAME, , uint, u, uint, 32, 2);
  TEST_VCOMP(INSN_NAME, q, uint, u, uint, 8, 16);
  TEST_VCOMP(INSN_NAME, q, uint, u, uint, 16, 8);
  TEST_VCOMP(INSN_NAME, q, uint, u, uint, 32, 4);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_uint, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_uint, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_uint, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_q_uint, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_q_uint, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_q_uint, "");

  /* The float variants.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_float, "");
#endif
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_float, "");

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VCOMP (INSN_NAME, q, float, f, uint, 16, 8);
  CHECK (TEST_MSG, uint, 16, 8, PRIx16, expected_q_float, "");
#endif
  TEST_VCOMP(INSN_NAME, q, float, f, uint, 32, 4);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_q_float, "");

  /* Some "special" input values to test some corner cases.  */
  /* Extra tests to have 100% coverage on all the variants.  */
  VDUP(vector2, , uint, u, 32, 2, 0xFFFFFFF0);
  TEST_VCOMP(INSN_NAME, , uint, u, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_uint2, "uint 0xfffffff0");

  VDUP(vector2, , int, s, 32, 2, -15);
  TEST_VCOMP(INSN_NAME, , int, s, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_uint3, "int -15");

  VDUP(vector2, , float, f, 32, 2, -16.0f);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_uint4, "float -16.0f");


  /* Extra FP tests with special values (NaN, ....).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP (vector, , float, f, 16, 4, 1.0);
  VDUP (vector2, , float, f, 16, 4, NAN);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_nan, "FP special (NaN)");

  VDUP (vector, , float, f, 16, 4, 1.0);
  VDUP (vector2, , float, f, 16, 4, -NAN);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_mnan, " FP special (-NaN)");

  VDUP (vector, , float, f, 16, 4, NAN);
  VDUP (vector2, , float, f, 16, 4, 1.0);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_nan2, " FP special (NaN)");

  VDUP (vector, , float, f, 16, 4, 1.0);
  VDUP (vector2, , float, f, 16, 4, HUGE_VALF);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_inf, " FP special (inf)");

  VDUP (vector, , float, f, 16, 4, 1.0);
  VDUP (vector2, , float, f, 16, 4, -HUGE_VALF);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_minf, " FP special (-inf)");

  VDUP (vector, , float, f, 16, 4, HUGE_VALF);
  VDUP (vector2, , float, f, 16, 4, 1.0);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_inf2, " FP special (inf)");

  VDUP (vector, , float, f, 16, 4, -0.0);
  VDUP (vector2, , float, f, 16, 4, 0.0);
  TEST_VCOMP (INSN_NAME, , float, f, uint, 16, 4);
  CHECK (TEST_MSG, uint, 16, 4, PRIx16, expected_mzero, " FP special (-0.0)");
#endif

  VDUP(vector, , float, f, 32, 2, 1.0);
  VDUP(vector2, , float, f, 32, 2, NAN);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_nan, "FP special (NaN)");

  VDUP(vector, , float, f, 32, 2, 1.0);
  VDUP(vector2, , float, f, 32, 2, -NAN);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_mnan, " FP special (-NaN)");

  VDUP(vector, , float, f, 32, 2, NAN);
  VDUP(vector2, , float, f, 32, 2, 1.0);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_nan2, " FP special (NaN)");

  VDUP(vector, , float, f, 32, 2, 1.0);
  VDUP(vector2, , float, f, 32, 2, HUGE_VALF);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_inf, " FP special (inf)");

  VDUP(vector, , float, f, 32, 2, 1.0);
  VDUP(vector2, , float, f, 32, 2, -HUGE_VALF);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_minf, " FP special (-inf)");

  VDUP(vector, , float, f, 32, 2, HUGE_VALF);
  VDUP(vector2, , float, f, 32, 2, 1.0);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_inf2, " FP special (inf)");

  VDUP(vector, , float, f, 32, 2, -0.0);
  VDUP(vector2, , float, f, 32, 2, 0.0);
  TEST_VCOMP(INSN_NAME, , float, f, uint, 32, 2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_mzero, " FP special (-0.0)");

#ifdef EXTRA_TESTS
  EXTRA_TESTS();
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
