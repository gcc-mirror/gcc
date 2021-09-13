#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_neg,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,32,2) [] = { 0x0, 0x0 };

/* Expected results with max input value shifted by 1.  */
VECT_VAR_DECL(expected_max_sh1,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_sh1,uint,16,4) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_sh1,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_max_sh1,uint,64,1) [] = { 0x3333333333333333 };

/* Expected results with max input value shifted by max amount.  */
VECT_VAR_DECL(expected_max_shmax,uint,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_shmax,uint,16,4) [] = { 0x8000, 0x8000,
						   0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_shmax,uint,32,2) [] = { 0x80000000, 0x80000000 };

/* Expected results with min input value shifted by max amount.  */
VECT_VAR_DECL(expected_min_shmax,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,uint,32,2) [] = { 0x0, 0x0 };

/* Expected results with inputs in usual range.  */
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x49, 0x49, 0x49, 0x49,
					0x49, 0x49, 0x49, 0x49 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xdeadbf, 0xdeadbf };

#define INSN vqrshrun_n
#define TEST_MSG "VQRSHRUN_N"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: y=vqrshrun_n(x,v), then store the result.  */
#define TEST_VQRSHRUN_N2(INSN, T1, T2, W, W2, N, V, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, uint, W2, N));	\
  VECT_VAR(vector_res, uint, W2, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),				\
		   V);							\
  vst1_u##W2(VECT_VAR(result, uint, W2, N),				\
	     VECT_VAR(vector_res, uint, W2, N))

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQRSHRUN_N1(INSN, T1, T2, W, W2, N, V, CMT) \
  TEST_VQRSHRUN_N2(INSN, T1, T2, W, W2, N, V, CMT)

#define TEST_VQRSHRUN_N(T1, T2, W, W2, N, V, CMT) \
  TEST_VQRSHRUN_N1(INSN, T1, T2, W, W2, N, V, CMT)


  /* vector is twice as large as vector_res.  */
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);

  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);

  clean_results ();

  /* Fill input vector with negative values, to check saturation on
     limits.  */
  VDUP(vector, q, int, s, 16, 8, -2);
  VDUP(vector, q, int, s, 32, 4, -3);
  VDUP(vector, q, int, s, 64, 2, -4);

  /* Choose shift amount arbitrarily.   */
#define CMT " (negative input)"
  TEST_VQRSHRUN_N(int, s, 16, 8, 8, 3, CMT);
  TEST_VQRSHRUN_N(int, s, 32, 16, 4, 4, CMT);
  TEST_VQRSHRUN_N(int, s, 64, 32, 2, 2, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg, CMT);


  /* Fill input vector with max value, to check saturation on
     limits.  */
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);

  /* shift by 1.  */
#undef CMT
#define CMT " (check cumulative saturation: shift by 1)"
  TEST_VQRSHRUN_N(int, s, 16, 8, 8, 1, CMT);
  TEST_VQRSHRUN_N(int, s, 32, 16, 4, 1, CMT);
  TEST_VQRSHRUN_N(int, s, 64, 32, 2, 1, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh1, CMT);


  /* shift by max.  */
#undef CMT
#define CMT " (check cumulative saturation: shift by max, positive input)"
  TEST_VQRSHRUN_N(int, s, 16, 8, 8, 8, CMT);
  TEST_VQRSHRUN_N(int, s, 32, 16, 4, 16, CMT);
  TEST_VQRSHRUN_N(int, s, 64, 32, 2, 32, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_shmax, CMT);


  /* Fill input vector with min value, to check saturation on limits.  */
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector, q, int, s, 64, 2, 0x8000000000000000LL);

  /* shift by max  */
#undef CMT
#define CMT " (check cumulative saturation: shift by max, negative input)"
  TEST_VQRSHRUN_N(int, s, 16, 8, 8, 8, CMT);
  TEST_VQRSHRUN_N(int, s, 32, 16, 4, 16, CMT);
  TEST_VQRSHRUN_N(int, s, 64, 32, 2, 32, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_min_shmax, CMT);


  /* Fill input vector with positive values, to check normal case.  */
  VDUP(vector, q, int, s, 16, 8, 0x1234);
  VDUP(vector, q, int, s, 32, 4, 0x87654321);
  VDUP(vector, q, int, s, 64, 2, 0xDEADBEEF);

  /* shift arbitrary amount.  */
#undef CMT
#define CMT ""
  TEST_VQRSHRUN_N(int, s, 16, 8, 8, 6, CMT);
  TEST_VQRSHRUN_N(int, s, 32, 16, 4, 7, CMT);
  TEST_VQRSHRUN_N(int, s, 64, 32, 2, 8, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
}

int main (void)
{
  exec_vqrshrun_n ();
  return 0;
}
