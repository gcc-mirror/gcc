#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x3, 0x3, 0x3, 0x3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x11, 0x11 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x5, 0x5 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2,
					0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x3, 0x3, 0x3, 0x3 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3,
					 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xd, 0xd, 0xd, 0xd,
					 0xd, 0xd, 0xd, 0xd };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x1f, 0x1f, 0x1f, 0x1f };


/* Expected results with input=0.  */
VECT_VAR_DECL(expected_with_0,int,8,8) [] = { 0x8, 0x8, 0x8, 0x8,
					      0x8, 0x8, 0x8, 0x8 };
VECT_VAR_DECL(expected_with_0,int,16,4) [] = { 0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_with_0,int,32,2) [] = { 0x20, 0x20 };
VECT_VAR_DECL(expected_with_0,uint,8,8) [] = { 0x8, 0x8, 0x8, 0x8,
					       0x8, 0x8, 0x8, 0x8 };
VECT_VAR_DECL(expected_with_0,uint,16,4) [] = { 0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_with_0,uint,32,2) [] = { 0x20, 0x20 };
VECT_VAR_DECL(expected_with_0,int,8,16) [] = { 0x8, 0x8, 0x8, 0x8,
					       0x8, 0x8, 0x8, 0x8,
					       0x8, 0x8, 0x8, 0x8,
					       0x8, 0x8, 0x8, 0x8 };
VECT_VAR_DECL(expected_with_0,int,16,8) [] = { 0x10, 0x10, 0x10, 0x10,
					       0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_with_0,int,32,4) [] = { 0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_with_0,uint,8,16) [] = { 0x8, 0x8, 0x8, 0x8,
						0x8, 0x8, 0x8, 0x8,
						0x8, 0x8, 0x8, 0x8,
						0x8, 0x8, 0x8, 0x8 };
VECT_VAR_DECL(expected_with_0,uint,16,8) [] = { 0x10, 0x10, 0x10, 0x10,
						0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_with_0,uint,32,4) [] = { 0x20, 0x20, 0x20, 0x20 };

#define INSN_NAME vclz
#define TEST_MSG "VCLZ/VCLZQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=vclz(x), then store the result.  */
#define TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_UNARY_OP(INSN, Q, T1, T2, W, N)	\
  TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)		\

  /* No need for 64 bits variants */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  clean_results ();

  /* Fill input vector with arbitrary values.  */
  VDUP(vector, , int, s, 8, 8, 0x84);
  VDUP(vector, , int, s, 16, 4, 0x1234);
  VDUP(vector, , int, s, 32, 2, 0x5678);
  VDUP(vector, , uint, u, 8, 8, 0x34);
  VDUP(vector, , uint, u, 16, 4, 0x8234);
  VDUP(vector, , uint, u, 32, 2, 0x7654321);
  VDUP(vector, q, int, s, 8, 16, 0x34);
  VDUP(vector, q, int, s, 16, 8, 0x1234);
  VDUP(vector, q, int, s, 32, 4, 0x12345678);
  VDUP(vector, q, uint, u, 8, 16, 0x13);
  VDUP(vector, q, uint, u, 16, 8, 0x4);
  VDUP(vector, q, uint, u, 32, 4, 0x1);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_UNARY_OP(INSN_NAME, , int, s, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , int, s, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , int, s, 32, 2);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 32, 2);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 32, 4);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 32, 4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");

  /* Test with zero as input.  */
  VDUP(vector, , int, s, 8, 8, 0);
  VDUP(vector, , int, s, 16, 4, 0);
  VDUP(vector, , int, s, 32, 2, 0);
  VDUP(vector, , uint, u, 8, 8, 0);
  VDUP(vector, , uint, u, 16, 4, 0);
  VDUP(vector, , uint, u, 32, 2, 0);
  VDUP(vector, q, int, s, 8, 16, 0);
  VDUP(vector, q, int, s, 16, 8, 0);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, uint, u, 8, 16, 0);
  VDUP(vector, q, uint, u, 16, 8, 0);
  VDUP(vector, q, uint, u, 32, 4, 0);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_UNARY_OP(INSN_NAME, , int, s, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , int, s, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , int, s, 32, 2);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 32, 2);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 32, 4);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 32, 4);

#define MSG_ZERO " (input=0)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_with_0, MSG_ZERO);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_with_0, MSG_ZERO);
}

int main (void)
{
  exec_vclz ();
  return 0;
}
