#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffe1, 0xffe5, 0xffe9, 0xffed };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffe1, 0xffffffe5 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffffe1 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x1e1, 0x1e5, 0x1e9, 0x1ed };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x1ffe1, 0x1ffe5 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x1ffffffe1 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffe1, 0xffe5, 0xffe9, 0xffed,
					0xfff1, 0xfff5, 0xfff9, 0xfffd };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffe1, 0xffffffe5,
					0xffffffe9, 0xffffffed };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffe1,
					0xffffffffffffffe5 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x1e1, 0x1e5, 0x1e9, 0x1ed,
					 0x1f1, 0x1f5, 0x1f9, 0x1fd };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x1ffe1, 0x1ffe5, 0x1ffe9, 0x1ffed };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x1ffffffe1, 0x1ffffffe5 };

#define INSN_NAME vpaddl
#define TEST_MSG "VPADDL/VPADDLQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_VPADDL1(INSN, Q, T1, T2, W, N, W2, N2)	\
  VECT_VAR(vector_res, T1, W2, N2) =			\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));	\
  vst1##Q##_##T2##W2(VECT_VAR(result, T1, W2, N2),	\
		    VECT_VAR(vector_res, T1, W2, N2))

#define TEST_VPADDL(INSN, Q, T1, T2, W, N, W2, N2)	\
  TEST_VPADDL1(INSN, Q, T1, T2, W, N, W2, N2)

  /* No need for 64 bits elements variants.  */
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

  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 64, 1);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 64, 1);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 8, 8);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
  VLOAD(vector, buffer, q, int, s, 8, 16);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 8, 16);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_VPADDL(INSN_NAME, , int, s, 8, 8, 16, 4);
  TEST_VPADDL(INSN_NAME, , int, s, 16, 4, 32, 2);
  TEST_VPADDL(INSN_NAME, , int, s, 32, 2, 64, 1);
  TEST_VPADDL(INSN_NAME, , uint, u, 8, 8, 16, 4);
  TEST_VPADDL(INSN_NAME, , uint, u, 16, 4, 32, 2);
  TEST_VPADDL(INSN_NAME, , uint, u, 32, 2, 64, 1);
  TEST_VPADDL(INSN_NAME, q, int, s, 8, 16, 16, 8);
  TEST_VPADDL(INSN_NAME, q, int, s, 16, 8, 32, 4);
  TEST_VPADDL(INSN_NAME, q, int, s, 32, 4, 64, 2);
  TEST_VPADDL(INSN_NAME, q, uint, u, 8, 16, 16, 8);
  TEST_VPADDL(INSN_NAME, q, uint, u, 16, 8, 32, 4);
  TEST_VPADDL(INSN_NAME, q, uint, u, 32, 4, 64, 2);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vpaddl ();
  return 0;
}
