#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfffc, 0xfffc, 0xfffc, 0xfffd };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffe, 0xfffffffe };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x6, 0x6, 0x6, 0x5,
					0x5, 0x4, 0x4, 0x4 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffe, 0xfffffffe,
					0xfffffffe, 0xfffffffe };

/* Expected results when multiplication saturates.  */
VECT_VAR_DECL(expected_mul,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_mul,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_mul,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
					    0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_mul,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					    0x7fffffff, 0x7fffffff };

/* Expected results when rounding should not cause saturation.  */
VECT_VAR_DECL(expected_round,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_round,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_round,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
					      0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_round,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					      0x7fffffff, 0x7fffffff };

#define INSN vqrdmulh
#define TEST_MSG "VQRDMULH_N"

#define FNNAME1(NAME) void exec_ ## NAME ## _n (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  int i;

  /* vector_res = vqrdmulh_n(vector,val), then store the result.  */
#define TEST_VQRDMULH_N2(INSN, Q, T1, T2, W, N, L, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			L);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N))

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQRDMULH_N1(INSN, Q, T1, T2, W, N, L, CMT) \
  TEST_VQRDMULH_N2(INSN, Q, T1, T2, W, N, L, CMT)

#define TEST_VQRDMULH_N(Q, T1, T2, W, N, L, CMT) \
  TEST_VQRDMULH_N1(INSN, Q, T1, T2, W, N, L, CMT)


  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);

  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);

  clean_results ();

  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);

  /* Choose multiplier arbitrarily.  */
#define CMT ""
  TEST_VQRDMULH_N(, int, s, 16, 4, 0x2233, CMT);
  TEST_VQRDMULH_N(, int, s, 32, 2, 0x12345678, CMT);
  TEST_VQRDMULH_N(q, int, s, 16, 8, 0xCD12, CMT);
  TEST_VQRDMULH_N(q, int, s, 32, 4, 0xFA23456, CMT);

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

  TEST_VQRDMULH_N(, int, s, 16, 4, 0x8000, TEST_MSG_MUL);
  TEST_VQRDMULH_N(, int, s, 32, 2, 0x80000000, TEST_MSG_MUL);
  TEST_VQRDMULH_N(q, int, s, 16, 8, 0x8000, TEST_MSG_MUL);
  TEST_VQRDMULH_N(q, int, s, 32, 4, 0x80000000, TEST_MSG_MUL);

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

  TEST_VQRDMULH_N(, int, s, 16, 4, 0x8001, TEST_MSG_ROUND);
  TEST_VQRDMULH_N(, int, s, 32, 2, 0x80000001, TEST_MSG_ROUND);
  TEST_VQRDMULH_N(q, int, s, 16, 8, 0x8001, TEST_MSG_ROUND);
  TEST_VQRDMULH_N(q, int, s, 32, 4, 0x80000001, TEST_MSG_ROUND);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_round, TEST_MSG_ROUND);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_round, TEST_MSG_ROUND);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_round, TEST_MSG_ROUND);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_round, TEST_MSG_ROUND);
}

int main (void)
{
  exec_vqrdmulh_n ();
  return 0;
}
