#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
					0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x33333333, 0x33333333,
					0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x3333333333333333, 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6,
					 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x33333333, 0x33333333,
					 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6,
					 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x33333333, 0x33333333,
					   0x33333333, 0x33333333 };

#define INSN_NAME vcnt
#define TEST_MSG "VCNT/VCNTQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=vcnt(x), then store the result.  */
#define TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_UNARY_OP(INSN, Q, T1, T2, W, N)	\
  TEST_UNARY_OP1(INSN, Q, T1, T2, W, N)		\

  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, poly, 8, 8);
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, poly, 8, 16);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, poly, 8, 8);
  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, poly, 8, 16);

  clean_results ();

  /* Fill input vector with arbitrary values.  */
  VDUP(vector, , int, s, 8, 8, 0xFF);
  VDUP(vector, , uint, u, 8, 8, 0x35);
  VDUP(vector, , poly, p, 8, 8, 0x35);
  VDUP(vector, q, int, s, 8, 16, 0);
  VDUP(vector, q, uint, u, 8, 16, 0xBD);
  VDUP(vector, q, poly, p, 8, 16, 0xBD);

  /* Apply a unary operator named INSN_NAME.  */
  TEST_UNARY_OP(INSN_NAME, , int, s, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , uint, u, 8, 8);
  TEST_UNARY_OP(INSN_NAME, , poly, p, 8, 8);
  TEST_UNARY_OP(INSN_NAME, q, int, s, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, uint, u, 8, 16);
  TEST_UNARY_OP(INSN_NAME, q, poly, p, 8, 16);

  CHECK_RESULTS (TEST_MSG, "");
}

int main (void)
{
  exec_vcnt ();
  return 0;
}
