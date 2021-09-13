/* Template file for saturating binary operator validation.

   This file is meant to be included by the relevant test files, which
   have to define the intrinsic family to test. If a given intrinsic
   supports variants which are not supported by all the other
   saturating binary operators, these can be tested by providing a
   definition for EXTRA_TESTS.  */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = OP(vector1,vector2), then store the result.  */

#define TEST_BINARY_SAT_OP1(INSN, Q, T1, T2, W, N, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
    vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),			\
		      VECT_VAR(vector_res, T1, W, N))

#define TEST_BINARY_SAT_OP(INSN, Q, T1, T2, W, N, CMT) \
  TEST_BINARY_SAT_OP1(INSN, Q, T1, T2, W, N, CMT)

  DECL_VARIABLE_ALL_VARIANTS(vector1);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector1, buffer);

  /* Choose arbitrary initialization values.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , int, s, 64, 1, 0x44);
  VDUP(vector2, , uint, u, 8, 8, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x66);
  VDUP(vector2, , uint, u, 32, 2, 0x77);
  VDUP(vector2, , uint, u, 64, 1, 0x88);

  VDUP(vector2, q, int, s, 8, 16, 0x11);
  VDUP(vector2, q, int, s, 16, 8, 0x22);
  VDUP(vector2, q, int, s, 32, 4, 0x33);
  VDUP(vector2, q, int, s, 64, 2, 0x44);
  VDUP(vector2, q, uint, u, 8, 16, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x66);
  VDUP(vector2, q, uint, u, 32, 4, 0x77);
  VDUP(vector2, q, uint, u, 64, 2, 0x88);

  /* Apply a saturating binary operator named INSN_NAME.  */
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 8, 8, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 16, 4, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 32, 2, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 8, 8, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 16, 4, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 32, 2, "");
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1, "");

  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 8, 16, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 16, 8, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 32, 4, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 8, 16, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 16, 8, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 32, 4, "");
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, "");

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");

#ifdef EXTRA_TESTS
  EXTRA_TESTS();
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
