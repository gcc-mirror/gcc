#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,8,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,uint,8,8) = 0;
int VECT_VAR(expected_cumulative_sat,uint,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,uint,32,2) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x12, 0x12, 0x12, 0x12,
				       0x12, 0x12, 0x12, 0x12 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x1278, 0x1278, 0x1278, 0x1278 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x12345678, 0x12345678 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x82, 0x82, 0x82, 0x82,
					0x82, 0x82, 0x82, 0x82 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x8765, 0x8765, 0x8765, 0x8765 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x87654321, 0x87654321 };

/* Expected values of cumulative_saturation flag when saturation occurs.  */
int VECT_VAR(expected_cumulative_sat1,int,8,8) = 1;
int VECT_VAR(expected_cumulative_sat1,int,16,4) = 1;
int VECT_VAR(expected_cumulative_sat1,int,32,2) = 1;
int VECT_VAR(expected_cumulative_sat1,uint,8,8) = 1;
int VECT_VAR(expected_cumulative_sat1,uint,16,4) = 1;
int VECT_VAR(expected_cumulative_sat1,uint,32,2) = 1;

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected1,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
					0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected1,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected1,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected1,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected1,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected1,uint,32,2) [] = { 0xffffffff, 0xffffffff };

#define INSN_NAME vqmovn
#define TEST_MSG "VQMOVN"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_UNARY_OP1(INSN, T1, T2, W, W2, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##_##T2##W2(VECT_VAR(vector, T1, W2, N));			\
  vst1##_##T2##W(VECT_VAR(result, T1, W, N),				\
		 VECT_VAR(vector_res, T1, W, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_UNARY_OP(INSN, T1, T2, W, W2, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_UNARY_OP1(INSN, T1, T2, W, W2, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, uint, 64, 2);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);

  clean_results ();

  /* Fill input vector with arbitrary values.  */
  VDUP(vector, q, int, s, 16, 8, 0x12);
  VDUP(vector, q, int, s, 32, 4, 0x1278);
  VDUP(vector, q, int, s, 64, 2, 0x12345678);
  VDUP(vector, q, uint, u, 16, 8, 0x82);
  VDUP(vector, q, uint, u, 32, 4, 0x8765);
  VDUP(vector, q, uint, u, 64, 2, 0x87654321);

  /* Apply a unary operator named INSN_NAME.  */
#define CMT ""
  TEST_UNARY_OP(INSN_NAME, int, s, 8, 16, 8, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, int, s, 16, 32, 4, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, int, s, 32, 64, 2, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 8, 16, 8, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 16, 32, 4, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 32, 64, 2, expected_cumulative_sat, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);


  /* Fill input vector with arbitrary values which cause cumulative
     saturation.  */
  VDUP(vector, q, int, s, 16, 8, 0x1234);
  VDUP(vector, q, int, s, 32, 4, 0x12345678);
  VDUP(vector, q, int, s, 64, 2, 0x1234567890ABLL);
  VDUP(vector, q, uint, u, 16, 8, 0x8234);
  VDUP(vector, q, uint, u, 32, 4, 0x87654321);
  VDUP(vector, q, uint, u, 64, 2, 0x8765432187654321ULL);

  /* Apply a unary operator named INSN_NAME.  */
#undef CMT
#define CMT " (with saturation)"
  TEST_UNARY_OP(INSN_NAME, int, s, 8, 16, 8, expected_cumulative_sat1, CMT);
  TEST_UNARY_OP(INSN_NAME, int, s, 16, 32, 4, expected_cumulative_sat1, CMT);
  TEST_UNARY_OP(INSN_NAME, int, s, 32, 64, 2, expected_cumulative_sat1, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 8, 16, 8, expected_cumulative_sat1, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 16, 32, 4, expected_cumulative_sat1, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 32, 64, 2, expected_cumulative_sat1, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected1, CMT);
}

int main (void)
{
  exec_vqmovn ();
  return 0;
}
