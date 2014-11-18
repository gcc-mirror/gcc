#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x19, 0x19 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x7, 0x7, 0x7, 0x7,
					0x7, 0x7, 0x7, 0x7,
					0x7, 0x7, 0x7, 0x7,
					0x7, 0x7, 0x7, 0x7 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x2, 0x2, 0x2, 0x2,
					0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x14, 0x14, 0x14, 0x14 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x3333333333333333,
					0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x33333333, 0x33333333,
					 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x33333333, 0x33333333,
					   0x33333333, 0x33333333 };

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_with_negative,int,8,8) [] = { 0x7, 0x7, 0x7, 0x7,
						     0x7, 0x7, 0x7, 0x7 };
VECT_VAR_DECL(expected_with_negative,int,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_with_negative,int,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_with_negative,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected_with_negative,uint,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
						      0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected_with_negative,uint,16,4) [] = { 0x3333, 0x3333,
						       0x3333, 0x3333 };
VECT_VAR_DECL(expected_with_negative,uint,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected_with_negative,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected_with_negative,poly,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
						      0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected_with_negative,poly,16,4) [] = { 0x3333, 0x3333,
						       0x3333, 0x3333 };
VECT_VAR_DECL(expected_with_negative,hfloat,32,2) [] = { 0x33333333,
							 0x33333333 };
VECT_VAR_DECL(expected_with_negative,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_with_negative,int,16,8) [] = { 0x2, 0x2, 0x2, 0x2,
						      0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected_with_negative,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_with_negative,int,64,2) [] = { 0x3333333333333333,
						      0x3333333333333333 };
VECT_VAR_DECL(expected_with_negative,uint,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
						       0x33, 0x33, 0x33, 0x33,
						       0x33, 0x33, 0x33, 0x33,
						       0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected_with_negative,uint,16,8) [] = { 0x3333, 0x3333,
						       0x3333, 0x3333,
						       0x3333, 0x3333,
						       0x3333, 0x3333 };
VECT_VAR_DECL(expected_with_negative,uint,32,4) [] = { 0x33333333, 0x33333333,
						       0x33333333, 0x33333333 };
VECT_VAR_DECL(expected_with_negative,uint,64,2) [] = { 0x3333333333333333,
						       0x3333333333333333 };
VECT_VAR_DECL(expected_with_negative,poly,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
						       0x33, 0x33, 0x33, 0x33,
						       0x33, 0x33, 0x33, 0x33,
						       0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected_with_negative,poly,16,8) [] = { 0x3333, 0x3333,
						       0x3333, 0x3333,
						       0x3333, 0x3333,
						       0x3333, 0x3333 };
VECT_VAR_DECL(expected_with_negative,hfloat,32,4) [] = { 0x33333333,
							 0x33333333,
							 0x33333333,
							 0x33333333 };

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

  CHECK_RESULTS (TEST_MSG, " (positive input)");

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

  CHECK_RESULTS_NAMED (TEST_MSG, expected_with_negative, " (negative input)");
}

int main (void)
{
  exec_vcls ();
  return 0;
}
