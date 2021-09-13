#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_neg,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_neg,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,64,2) [] = { 0x0, 0x0 };

/* Expected results with shift by 1.  */
VECT_VAR_DECL(expected_sh1,uint,8,8) [] = { 0xfe, 0xfe, 0xfe, 0xfe,
					    0xfe, 0xfe, 0xfe, 0xfe };
VECT_VAR_DECL(expected_sh1,uint,16,4) [] = { 0xfffe, 0xfffe, 0xfffe, 0xfffe };
VECT_VAR_DECL(expected_sh1,uint,32,2) [] = { 0xfffffffe, 0xfffffffe };
VECT_VAR_DECL(expected_sh1,uint,64,1) [] = { 0xfffffffffffffffe };
VECT_VAR_DECL(expected_sh1,uint,8,16) [] = { 0xfe, 0xfe, 0xfe, 0xfe,
					     0xfe, 0xfe, 0xfe, 0xfe,
					     0xfe, 0xfe, 0xfe, 0xfe,
					     0xfe, 0xfe, 0xfe, 0xfe };
VECT_VAR_DECL(expected_sh1,uint,16,8) [] = { 0xfffe, 0xfffe, 0xfffe, 0xfffe,
					     0xfffe, 0xfffe, 0xfffe, 0xfffe };
VECT_VAR_DECL(expected_sh1,uint,32,4) [] = { 0xfffffffe, 0xfffffffe,
					     0xfffffffe, 0xfffffffe };
VECT_VAR_DECL(expected_sh1,uint,64,2) [] = { 0xfffffffffffffffe,
					     0xfffffffffffffffe };

/* Expected results with shift by 2.  */
VECT_VAR_DECL(expected_sh2,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_sh2,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_sh2,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_sh2,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_sh2,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_sh2,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					     0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_sh2,uint,32,4) [] = { 0xffffffff, 0xffffffff,
					     0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_sh2,uint,64,2) [] = { 0xffffffffffffffff,
					     0xffffffffffffffff };

/* Expected results.  */
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x8, 0x8, 0x8, 0x8 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x18, 0x18 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x40 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xa0, 0xa0, 0xa0, 0xa0,
					 0xa0, 0xa0, 0xa0, 0xa0,
					 0xa0, 0xa0, 0xa0, 0xa0,
					 0xa0, 0xa0, 0xa0, 0xa0 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x180, 0x180, 0x180, 0x180,
					 0x180, 0x180, 0x180, 0x180 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x380, 0x380, 0x380, 0x380 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x800, 0x800 };


#define INSN vqshlu
#define TEST_MSG "VQSHLU_N/VQSHLUQ_N"

#define FNNAME1(NAME) void exec_ ## NAME ## _n(void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: v2=vqshlu_n(v1,v), then store the result.  */
#define TEST_VQSHLU_N2(INSN, Q, T1, T2, T3, T4, W, N, V, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T3, W, N));		\
  VECT_VAR(vector_res, T3, W, N) =					\
    INSN##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			V);						\
  vst1##Q##_##T4##W(VECT_VAR(result, T3, W, N),				\
		    VECT_VAR(vector_res, T3, W, N))

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHLU_N1(INSN, Q, T1, T2, T3, T4, W, N, V, CMT) \
  TEST_VQSHLU_N2(INSN, Q, T1, T2, T3, T4, W, N, V, CMT)

#define TEST_VQSHLU_N(Q, T1, T2, T3, T4, W, N, V, CMT) \
  TEST_VQSHLU_N1(INSN, Q, T1, T2, T3, T4, W, N, V, CMT)


  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Fill input vector with negative values, to check saturation on
     limits.  */
  VDUP(vector, , int, s, 8, 8, -1);
  VDUP(vector, , int, s, 16, 4, -2);
  VDUP(vector, , int, s, 32, 2, -3);
  VDUP(vector, , int, s, 64, 1, -4);
  VDUP(vector, q, int, s, 8, 16, -1);
  VDUP(vector, q, int, s, 16, 8, -2);
  VDUP(vector, q, int, s, 32, 4, -3);
  VDUP(vector, q, int, s, 64, 2, -4);

  /* Choose shift amount arbitrarily.  */
#define CMT " (negative input)"
  TEST_VQSHLU_N(, int, s, uint, u, 8, 8, 2, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 16, 4, 1, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 32, 2, 1, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 64, 1, 2, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 8, 16, 2, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 16, 8, 1, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 32, 4, 1, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 64, 2, 2, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_neg, CMT);

  
  /* Fill input vector with max value, to check saturation on
     limits.  */
  VDUP(vector, , int, s, 8, 8, 0x7F);
  VDUP(vector, , int, s, 16, 4, 0x7FFF);
  VDUP(vector, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector, , int, s, 64, 1, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, q, int, s, 8, 16, 0x7F);
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFULL);

  /* shift by 1.  */
#undef CMT
#define CMT " (shift by 1)"
  TEST_VQSHLU_N(, int, s, uint, u, 8, 8, 1, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 16, 4, 1, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 32, 2, 1, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 64, 1, 1, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 8, 16, 1, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 16, 8, 1, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 32, 4, 1, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 64, 2, 1, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_sh1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_sh1, CMT);

  /* shift by 2 to force saturation.  */
#undef CMT
#define CMT " (shift by 2)"
  TEST_VQSHLU_N(, int, s, uint, u, 8, 8, 2, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 16, 4, 2, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 32, 2, 2, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 64, 1, 2, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 8, 16, 2, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 16, 8, 2, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 32, 4, 2, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 64, 2, 2, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_sh2, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_sh2, CMT);

  
  /* Fill input vector with positive values, to check normal case.  */
  VDUP(vector, , int, s, 8, 8, 1);
  VDUP(vector, , int, s, 16, 4, 2);
  VDUP(vector, , int, s, 32, 2, 3);
  VDUP(vector, , int, s, 64, 1, 4);
  VDUP(vector, q, int, s, 8, 16, 5);
  VDUP(vector, q, int, s, 16, 8, 6);
  VDUP(vector, q, int, s, 32, 4, 7);
  VDUP(vector, q, int, s, 64, 2, 8);

  /* Arbitrary shift amount.  */
#undef CMT
#define CMT ""
  TEST_VQSHLU_N(, int, s, uint, u, 8, 8, 1, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 16, 4, 2, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 32, 2, 3, CMT);
  TEST_VQSHLU_N(, int, s, uint, u, 64, 1, 4, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 8, 16, 5, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 16, 8, 6, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 32, 4, 7, CMT);
  TEST_VQSHLU_N(q, int, s, uint, u, 64, 2, 8, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, CMT);
}

int main (void)
{
  exec_vqshlu_n ();
  return 0;
}
