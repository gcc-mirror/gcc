/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_neg,uint,8,16) [] = { 0xfe, 0xfe, 0xfe, 0xfe,
					     0xfe, 0xfe, 0xfe, 0xfe,
					     0x0, 0x0, 0x0, 0x0,
					     0x0,0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,16,8) [] = { 0xfffd, 0xfffd, 0xfffd, 0xfffd,
					     0x0, 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,32,4) [] = { 0xfffffffc, 0xfffffffc,
					     0x0, 0x0 };

/* Expected results with max input value shifted by 1.  */
VECT_VAR_DECL(expected_max_sh1,uint,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						 0x7f, 0x7f, 0x7f, 0x7f,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_sh1,uint,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
						 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_sh1,uint,32,4) [] = { 0x7fffffff, 0x7fffffff,
						 0xffffffff, 0xffffffff };

/* Expected results.  */
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x12, 0x12, 0x12, 0x12,
					 0x12, 0x12, 0x12, 0x12,
					 0x48, 0x48, 0x48, 0x48,
					 0x48, 0x48, 0x48, 0x48 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x4321, 0x4321, 0x4321, 0x4321,
					 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xdeadbeef, 0xdeadbeef,
					 0xdeadbe, 0xdeadbe };

#define INSN vqshrun_high_n
#define TEST_MSG "VQSHRUN_HIGH_N"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: y=vqshrun_high_n(x,v), then store the result.  */
#define TEST_VQSHRUN_HIGH_N2(INSN, T1, T2, W, W2, N, N2, V) \
  VECT_VAR(vector_res, uint, W2, N2) =			    \
    INSN##_##T2##W(VECT_VAR(vector1,uint, W2, N),	    \
		   VECT_VAR(vector2, T1, W, N), V);	    \
  vst1q_u##W2(VECT_VAR(result, uint, W2, N2),		    \
	      VECT_VAR(vector_res, uint, W2, N2));	    \

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHRUN_HIGH_N1(INSN, T1, T2, W, W2, N, N2, V) \
  TEST_VQSHRUN_HIGH_N2(INSN, T1, T2, W, W2, N, N2, V)

#define TEST_VQSHRUN_HIGH_N(T1, T2, W, W2, N, N2, V) \
  TEST_VQSHRUN_HIGH_N1(INSN, T1, T2, W, W2, N, N2, V)


  DECL_VARIABLE(vector1, uint, 8, 8);
  DECL_VARIABLE(vector1, uint, 16, 4);
  DECL_VARIABLE(vector1, uint, 32, 2);

  /* vector is twice as large as vector_res.  */
  DECL_VARIABLE(vector2, int, 16, 8);
  DECL_VARIABLE(vector2, int, 32, 4);
  DECL_VARIABLE(vector2, int, 64, 2);

  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  clean_results ();

  /* Fill input vector with negative values, to check saturation on
     limits.  */
  VDUP(vector1, , uint, u, 8, 8, -2);
  VDUP(vector1, , uint, u, 16, 4, -3);
  VDUP(vector1, , uint, u, 32, 2, -4);

  VDUP(vector2, q, int, s, 16, 8, -2);
  VDUP(vector2, q, int, s, 32, 4, -3);
  VDUP(vector2, q, int, s, 64, 2, -4);

  /* Choose shift amount arbitrarily.  */
#define CMT " (negative input)"
  TEST_VQSHRUN_HIGH_N(int, s, 16, 8, 8, 16, 3);
  TEST_VQSHRUN_HIGH_N(int, s, 32, 16, 4, 8, 4);
  TEST_VQSHRUN_HIGH_N(int, s, 64, 32, 2, 4, 2);

  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_neg, CMT);


  /* Fill input vector with max value, to check saturation on
     limits.  */
  VDUP(vector1, , uint, u, 8, 8, 0x7F);
  VDUP(vector1, , uint, u, 16, 4, 0x7FFF);
  VDUP(vector1, , uint, u, 32, 2, 0x7FFFFFFFLL);

  VDUP(vector2, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector2, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector2, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);

#undef CMT
#define CMT " (check cumulative saturation)"
  TEST_VQSHRUN_HIGH_N(int, s, 16, 8, 8, 16, 1);
  TEST_VQSHRUN_HIGH_N(int, s, 32, 16, 4, 8, 1);
  TEST_VQSHRUN_HIGH_N(int, s, 64, 32, 2, 4, 1);

  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh1, CMT);


  /* Fill input vector with positive values, to check normal case.  */
  VDUP(vector1, , uint, u, 8, 8, 0x12);
  VDUP(vector1, , uint, u, 16, 4, 0x4321);
  VDUP(vector1, , uint, u, 32, 2, 0xDEADBEEF);

  VDUP(vector2, q, int, s, 16, 8, 0x1234);
  VDUP(vector2, q, int, s, 32, 4, 0x87654321);
  VDUP(vector2, q, int, s, 64, 2, 0xDEADBEEF);

#undef CMT
#define CMT ""
  TEST_VQSHRUN_HIGH_N(int, s, 16, 8, 8, 16, 6);
  TEST_VQSHRUN_HIGH_N(int, s, 32, 16, 4, 8, 7);
  TEST_VQSHRUN_HIGH_N(int, s, 64, 32, 2, 4, 8);

  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
}

int main (void)
{
  exec_vqshrun_high_n ();
  return 0;
}
