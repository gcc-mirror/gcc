#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x19, 0x19 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x7, 0x7, 0x7, 0x7,
					0x7, 0x7, 0x7, 0x7,
					0x7, 0x7, 0x7, 0x7,
					0x7, 0x7, 0x7, 0x7 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x2, 0x2, 0x2, 0x2,
					0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x14, 0x14, 0x14, 0x14 };

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_with_negative,int,8,8) [] = { 0x7, 0x7, 0x7, 0x7,
						     0x7, 0x7, 0x7, 0x7 };
VECT_VAR_DECL(expected_with_negative,int,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_with_negative,int,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_with_negative,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_with_negative,int,16,8) [] = { 0x2, 0x2, 0x2, 0x2,
						      0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected_with_negative,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

#define INSN_NAME vcls
#define TEST_MSG "VCLS/VCLSQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=vcls(x), then store the result.  */
#define TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_UNARY_OP(INSN, Q, T1, T2, W, N)	\
  TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)		\

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

  /* Fill input vector with arbitrary values.  */
  VDUP(vector, , int, s, 8, 8, 0x1);
  VDUP(vector, , int, s, 16, 4, 0x1234);
  VDUP(vector, , int, s, 32, 2, 0x34);
  VDUP(vector, q, int, s, 8, 16, 0);
  VDUP(vector, q, int, s, 16, 8, 0x1234);
  VDUP(vector, q, int, s, 32, 4, 0x678);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_UNARY_OP(INSN_NAME, , int, s, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , int, s, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , int, s, 32, 2);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 32, 4);

#define MSG_POSITIVE " (positive input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, MSG_POSITIVE);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, MSG_POSITIVE);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, MSG_POSITIVE);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, MSG_POSITIVE);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, MSG_POSITIVE);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, MSG_POSITIVE);

  /* Fill input vector with arbitrary values (negative).  */
  VDUP(vector, , int, s, 8, 8, 0xFF);
  VDUP(vector, , int, s, 16, 4, 0xC234);
  VDUP(vector, , int, s, 32, 2, 0xDEAD0034);
  VDUP(vector, q, int, s, 8, 16, 0x80);
  VDUP(vector, q, int, s, 16, 8, 0xE234);
  VDUP(vector, q, int, s, 32, 4, 0xBEEF0678);

  /* Apply a unary operator named INSN_NAME  */
  TEST_UNARY_OP(INSN_NAME, , int, s, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , int, s, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , int, s, 32, 2);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 32, 4);

#define MSG_NEGATIVE " (negative input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_with_negative, MSG_NEGATIVE);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_with_negative, MSG_NEGATIVE);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_with_negative, MSG_NEGATIVE);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_with_negative, MSG_NEGATIVE);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_with_negative, MSG_NEGATIVE);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_with_negative, MSG_NEGATIVE);
}

int main (void)
{
  exec_vcls ();
  return 0;
}
