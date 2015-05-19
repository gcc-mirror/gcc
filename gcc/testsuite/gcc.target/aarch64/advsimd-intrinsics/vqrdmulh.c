#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff5, 0xfff6, 0xfff7, 0xfff7 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected values of cumulative_saturation flag when multiplication
   saturates.  */
int VECT_VAR(expected_cumulative_sat_mul,int,16,4) = 1;
int VECT_VAR(expected_cumulative_sat_mul,int,32,2) = 1;
int VECT_VAR(expected_cumulative_sat_mul,int,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_mul,int,32,4) = 1;

/* Expected results when multiplication saturates.  */
VECT_VAR_DECL(expected_mul,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_mul,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_mul,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
					    0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_mul,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					    0x7fffffff, 0x7fffffff };

/* Expected values of cumulative_saturation flag when rounding
   should not cause saturation.  */
int VECT_VAR(expected_cumulative_sat_round,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat_round,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat_round,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat_round,int,32,4) = 0;

/* Expected results when rounding should not cause saturation.  */
VECT_VAR_DECL(expected_round,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_round,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_round,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
					      0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_round,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					      0x7fffffff, 0x7fffffff };

#define INSN vqrdmulh
#define TEST_MSG "VQRDMULH"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* vector_res = vqrdmulh(vector,vector2), then store the result.  */
#define TEST_VQRDMULH2(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQRDMULH1(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQRDMULH2(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_VQRDMULH(Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)	\
  TEST_VQRDMULH1(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)


  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);

  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);

  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, int, 16, 8);
  DECL_VARIABLE(vector2, int, 32, 4);

  clean_results ();

  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);

  /* Initialize vector2.  */
  VDUP(vector2, , int, s, 16, 4, 0x5555);
  VDUP(vector2, , int, s, 32, 2, 0xBB);
  VDUP(vector2, q, int, s, 16, 8, 0x33);
  VDUP(vector2, q, int, s, 32, 4, 0x22);

#define CMT ""
  TEST_VQRDMULH(, int, s, 16, 4, expected_cumulative_sat, CMT);
  TEST_VQRDMULH(, int, s, 32, 2, expected_cumulative_sat, CMT);
  TEST_VQRDMULH(q, int, s, 16, 8, expected_cumulative_sat, CMT);
  TEST_VQRDMULH(q, int, s, 32, 4, expected_cumulative_sat, CMT);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);

  /* Now use input values such that the multiplication causes
     saturation.  */
#define TEST_MSG_MUL " (check mul cumulative saturation)"
  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector2, , int, s, 16, 4, 0x8000);
  VDUP(vector2, , int, s, 32, 2, 0x80000000);
  VDUP(vector2, q, int, s, 16, 8, 0x8000);
  VDUP(vector2, q, int, s, 32, 4, 0x80000000);

  TEST_VQRDMULH(, int, s, 16, 4, expected_cumulative_sat_mul, TEST_MSG_MUL);
  TEST_VQRDMULH(, int, s, 32, 2, expected_cumulative_sat_mul, TEST_MSG_MUL);
  TEST_VQRDMULH(q, int, s, 16, 8, expected_cumulative_sat_mul, TEST_MSG_MUL);
  TEST_VQRDMULH(q, int, s, 32, 4, expected_cumulative_sat_mul, TEST_MSG_MUL);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_mul, TEST_MSG_MUL);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_mul, TEST_MSG_MUL);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_mul, TEST_MSG_MUL);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_mul, TEST_MSG_MUL);

  /* Use input values where rounding produces a result equal to the
     saturation value, but does not set the saturation flag.  */
#define TEST_MSG_ROUND " (check rounding)"
  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector2, , int, s, 16, 4, 0x8001);
  VDUP(vector2, , int, s, 32, 2, 0x80000001);
  VDUP(vector2, q, int, s, 16, 8, 0x8001);
  VDUP(vector2, q, int, s, 32, 4, 0x80000001);

  TEST_VQRDMULH(, int, s, 16, 4, expected_cumulative_sat_round, TEST_MSG_ROUND);
  TEST_VQRDMULH(, int, s, 32, 2, expected_cumulative_sat_round, TEST_MSG_ROUND);
  TEST_VQRDMULH(q, int, s, 16, 8, expected_cumulative_sat_round, TEST_MSG_ROUND);
  TEST_VQRDMULH(q, int, s, 32, 4, expected_cumulative_sat_round, TEST_MSG_ROUND);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_round, TEST_MSG_ROUND);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_round, TEST_MSG_ROUND);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_round, TEST_MSG_ROUND);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_round, TEST_MSG_ROUND);
}

int main (void)
{
  exec_vqrdmulh ();
  return 0;
}
