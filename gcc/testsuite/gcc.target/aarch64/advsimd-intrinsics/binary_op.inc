/* Template file for binary operator validation.

   This file is meant to be included by the relevant test files, which
   have to define the intrinsic family to test. If a given intrinsic
   supports variants which are not supported by all the other binary
   operators, these can be tested by providing a definition for
   EXTRA_TESTS.  */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=OP(x1,x2), then store the result.  */
#define TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_BINARY_OP(INSN, Q, T1, T2, W, N)				\
  TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)				\

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Fill input vector2 with arbitrary values.  */
  VDUP(vector2, , int, s, 8, 8, 2);
  VDUP(vector2, , int, s, 16, 4, -4);
  VDUP(vector2, , int, s, 32, 2, 3);
  VDUP(vector2, , int, s, 64, 1, 100);
  VDUP(vector2, , uint, u, 8, 8, 20);
  VDUP(vector2, , uint, u, 16, 4, 30);
  VDUP(vector2, , uint, u, 32, 2, 40);
  VDUP(vector2, , uint, u, 64, 1, 2);
  VDUP(vector2, q, int, s, 8, 16, -10);
  VDUP(vector2, q, int, s, 16, 8, -20);
  VDUP(vector2, q, int, s, 32, 4, -30);
  VDUP(vector2, q, int, s, 64, 2, 24);
  VDUP(vector2, q, uint, u, 8, 16, 12);
  VDUP(vector2, q, uint, u, 16, 8, 3);
  VDUP(vector2, q, uint, u, 32, 4, 55);
  VDUP(vector2, q, uint, u, 64, 2, 3);

  /* Apply a binary operator named INSN_NAME.  */
  TEST_MACRO_ALL_VARIANTS_1_5(TEST_BINARY_OP, INSN_NAME);

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
