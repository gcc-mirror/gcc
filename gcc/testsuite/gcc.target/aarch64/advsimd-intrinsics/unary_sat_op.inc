/* Template file for saturating unary operator validation.

   This file is meant to be included by the relevant test files, which
   have to define the intrinsic family to test. If a given intrinsic
   supports variants which are not supported by all the other
   saturating unary operators, these can be tested by providing a
   definition for EXTRA_TESTS.  */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* y=OP(x), then store the result.  */
#define TEST_UNARY_SAT_OP1(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
    vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),			\
		      VECT_VAR(vector_res, T1, W, N));			\
      CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_UNARY_SAT_OP(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_UNARY_SAT_OP1(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, q, int, s, 8, 16);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);

  /* Apply a saturating unary operator named INSN_NAME.  */
  TEST_UNARY_SAT_OP(INSN_NAME, , int, s, 8, 8, expected_cumulative_sat, "");
  TEST_UNARY_SAT_OP(INSN_NAME, , int, s, 16, 4, expected_cumulative_sat, "");
  TEST_UNARY_SAT_OP(INSN_NAME, , int, s, 32, 2, expected_cumulative_sat, "");
  TEST_UNARY_SAT_OP(INSN_NAME, q, int, s, 8, 16, expected_cumulative_sat, "");
  TEST_UNARY_SAT_OP(INSN_NAME, q, int, s, 16, 8, expected_cumulative_sat, "");
  TEST_UNARY_SAT_OP(INSN_NAME, q, int, s, 32, 4, expected_cumulative_sat, "");

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");

#ifdef EXTRA_TESTS
  EXTRA_TESTS();
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
