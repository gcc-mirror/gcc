#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,uint,8,8) = 0;
int VECT_VAR(expected_cumulative_sat,uint,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,uint,32,2) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x34, 0x34, 0x34, 0x34,
					0x34, 0x34, 0x34, 0x34 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x5678, 0x5678, 0x5678, 0x5678 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x12345678, 0x12345678 };

/* Expected values of cumulative_saturation flag with negative input.  */
int VECT_VAR(expected_cumulative_sat_neg,uint,8,8) = 1;
int VECT_VAR(expected_cumulative_sat_neg,uint,16,4) = 1;
int VECT_VAR(expected_cumulative_sat_neg,uint,32,2) = 1;

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_neg,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,32,2) [] = { 0x0, 0x0 };

#define INSN_NAME vqmovun
#define TEST_MSG "VQMOVUN"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_UNARY_OP1(INSN, T1, T2, W, W2, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##_s##W2(VECT_VAR(vector, int, W2, N));				\
  vst1##_##T2##W(VECT_VAR(result, T1, W, N),				\
		 VECT_VAR(vector_res, T1, W, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_UNARY_OP(INSN, T1, T2, W, W2, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_UNARY_OP1(INSN, T1, T2, W, W2, N, EXPECTED_CUMULATIVE_SAT, CMT)

  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);

  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);

  clean_results ();

  /* Fill input vector with arbitrary values.  */
  VDUP(vector, q, int, s, 16, 8, 0x34);
  VDUP(vector, q, int, s, 32, 4, 0x5678);
  VDUP(vector, q, int, s, 64, 2, 0x12345678);

  /* Apply a unary operator named INSN_NAME.  */
#define CMT ""
  TEST_UNARY_OP(INSN_NAME, uint, u, 8, 16, 8, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 16, 32, 4, expected_cumulative_sat, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 32, 64, 2, expected_cumulative_sat, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);

  /* Fill input vector with negative values.  */
  VDUP(vector, q, int, s, 16, 8, 0x8234);
  VDUP(vector, q, int, s, 32, 4, 0x87654321);
  VDUP(vector, q, int, s, 64, 2, 0x8765432187654321LL);

  /* Apply a unary operator named INSN_NAME.  */
#undef CMT
#define CMT " (negative input)"
  TEST_UNARY_OP(INSN_NAME, uint, u, 8, 16, 8, expected_cumulative_sat_neg, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 16, 32, 4, expected_cumulative_sat_neg, CMT);
  TEST_UNARY_OP(INSN_NAME, uint, u, 32, 64, 2, expected_cumulative_sat_neg, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg, CMT);
}

int main (void)
{
  exec_vqmovun ();
  return 0;
}
