#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf, 0xe, 0xd, 0xc,
				       0xb, 0xa, 0x9, 0x8 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xf, 0xe, 0xd, 0xc };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xf, 0xe };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf, 0xe, 0xd, 0xc,
					0xb, 0xa, 0x9, 0x8 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xf, 0xe, 0xd, 0xc };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xf, 0xe };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf, 0xe, 0xd, 0xc,
					0xb, 0xa, 0x9, 0x8 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf, 0xe, 0xd, 0xc,
					0xb, 0xa, 0x9, 0x8,
					0x7, 0x6, 0x5, 0x4,
					0x3, 0x2, 0x1, 0x0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xf, 0xe, 0xd, 0xc,
					0xb, 0xa, 0x9, 0x8 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xf, 0xe, 0xd, 0xc };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf, 0xe, 0xd, 0xc,
					 0xb, 0xa, 0x9, 0x8,
					 0x7, 0x6, 0x5, 0x4,
					 0x3, 0x2, 0x1, 0x0 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xf, 0xe, 0xd, 0xc,
					 0xb, 0xa, 0x9, 0x8 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xf, 0xe, 0xd, 0xc };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf, 0xe, 0xd, 0xc,
					 0xb, 0xa, 0x9, 0x8,
					 0x7, 0x6, 0x5, 0x4,
					 0x3, 0x2, 0x1, 0x0 };

#define INSN_NAME vmvn
#define TEST_MSG "VMVN/VMVNQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_UNARY_OP(INSN, Q, T1, T2, W, N)				\
  TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)					\

  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector, poly, 8, 8);
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, poly, 8, 16);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, poly, 8, 8);
  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, poly, 8, 16);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 8, 8);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
  VLOAD(vector, buffer, , poly, p, 8, 8);
  VLOAD(vector, buffer, q, int, s, 8, 16);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 8, 16);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, poly, p, 8, 16);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_UNARY_OP(INSN_NAME, , int, s, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , int, s, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , int, s, 32, 2);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 16, 4);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 32, 2);
  TEST_UNARY_OP(INSN_NAME, , poly, p, 8, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 32, 4);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 16, 8);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 32, 4);
  TEST_UNARY_OP(INSN_NAME, q, poly, p, 8, 16);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected, "");
}

int main (void)
{
  exec_vmvn ();
  return 0;
}
