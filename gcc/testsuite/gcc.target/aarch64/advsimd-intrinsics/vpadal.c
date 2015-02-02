#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffd1, 0xffd6, 0xffdb, 0xffe0 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffd1, 0xffffffd6 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffffd1 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x1d1, 0x1d6, 0x1db, 0x1e0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x1ffd1, 0x1ffd6 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x1ffffffd1 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffd1, 0xffd6, 0xffdb, 0xffe0,
					0xffe5, 0xffea, 0xffef, 0xfff4 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffd1, 0xffffffd6,
					0xffffffdb, 0xffffffe0 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffd1, 0xffffffffffffffd6 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x1d1, 0x1d6, 0x1db, 0x1e0,
					 0x1e5, 0x1ea, 0x1ef, 0x1f4 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x1ffd1, 0x1ffd6, 0x1ffdb, 0x1ffe0 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x1ffffffd1, 0x1ffffffd6 };

#define INSN_NAME vpadal
#define TEST_MSG "VPADAL/VPADALQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_VPADAL1(INSN, Q, T1, T2, W, N, W2, N2)			\
  VECT_VAR(vector_res, T1, W2, N2) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W2, N2), VECT_VAR(vector2, T1, W, N)); \
  vst1##Q##_##T2##W2(VECT_VAR(result, T1, W2, N2),			\
		     VECT_VAR(vector_res, T1, W2, N2))

#define TEST_VPADAL(INSN, Q, T1, T2, W, N, W2, N2)	\
  TEST_VPADAL1(INSN, Q, T1, T2, W, N, W2, N2)

  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, int, 64, 1);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector, uint, 64, 1);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, uint, 64, 2);

  DECL_VARIABLE(vector2, int, 8, 8);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, uint, 8, 8);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector2, uint, 32, 2);
  DECL_VARIABLE(vector2, int, 8, 16);
  DECL_VARIABLE(vector2, int, 16, 8);
  DECL_VARIABLE(vector2, int, 32, 4);
  DECL_VARIABLE(vector2, uint, 8, 16);
  DECL_VARIABLE(vector2, uint, 16, 8);
  DECL_VARIABLE(vector2, uint, 32, 4);

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
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , int, s, 64, 1);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
  VLOAD(vector, buffer, , uint, u, 64, 1);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  /* Initialize input "vector2" from "buffer".  */
  VLOAD(vector2, buffer, , int, s, 8, 8);
  VLOAD(vector2, buffer, , int, s, 16, 4);
  VLOAD(vector2, buffer, , int, s, 32, 2);
  VLOAD(vector2, buffer, , uint, u, 8, 8);
  VLOAD(vector2, buffer, , uint, u, 16, 4);
  VLOAD(vector2, buffer, , uint, u, 32, 2);
  VLOAD(vector2, buffer, q, int, s, 8, 16);
  VLOAD(vector2, buffer, q, int, s, 16, 8);
  VLOAD(vector2, buffer, q, int, s, 32, 4);
  VLOAD(vector2, buffer, q, uint, u, 8, 16);
  VLOAD(vector2, buffer, q, uint, u, 16, 8);
  VLOAD(vector2, buffer, q, uint, u, 32, 4);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_VPADAL(INSN_NAME, , int, s, 8, 8, 16, 4);
  TEST_VPADAL(INSN_NAME, , int, s, 16, 4, 32, 2);
  TEST_VPADAL(INSN_NAME, , int, s, 32, 2, 64 ,1);
  TEST_VPADAL(INSN_NAME, , uint, u, 8, 8, 16, 4);
  TEST_VPADAL(INSN_NAME, , uint, u, 16, 4, 32, 2);
  TEST_VPADAL(INSN_NAME, , uint, u, 32, 2, 64, 1);
  TEST_VPADAL(INSN_NAME, q, int, s, 8, 16, 16, 8);
  TEST_VPADAL(INSN_NAME, q, int, s, 16, 8, 32, 4);
  TEST_VPADAL(INSN_NAME, q, int, s, 32, 4, 64 ,2);
  TEST_VPADAL(INSN_NAME, q, uint, u, 8, 16, 16, 8);
  TEST_VPADAL(INSN_NAME, q, uint, u, 16, 8, 32, 4);
  TEST_VPADAL(INSN_NAME, q, uint, u, 32, 4, 64, 2);

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
  exec_vpadal ();
  return 0;
}
