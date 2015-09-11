#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag with negative input.  */
int VECT_VAR(expected_cumulative_sat_neg,int,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_neg,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat_neg,int,64,2) = 1;

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_neg,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,32,2) [] = { 0x0, 0x0 };

/* Expected values of cumulative_saturation flag with max input value
   shifted by 1.  */
int VECT_VAR(expected_cumulative_sat_max_sh1,int,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh1,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh1,int,64,2) = 1;

/* Expected results with max input value shifted by 1.  */
VECT_VAR_DECL(expected_max_sh1,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_sh1,uint,16,4) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_sh1,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_max_sh1,uint,64,1) [] = { 0x3333333333333333 };

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat,int,64,2) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x48, 0x48, 0x48, 0x48,
					0x48, 0x48, 0x48, 0x48 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xdeadbe, 0xdeadbe };


#define INSN vqshrun_n
#define TEST_MSG "VQSHRUN_N"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: y=vqshrun_n(x,v), then store the result.  */
#define TEST_VQSHRUN_N2(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, uint, W2, N));	\
  VECT_VAR(vector_res, uint, W2, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),				\
		   V);							\
  vst1_u##W2(VECT_VAR(result, uint, W2, N),				\
	     VECT_VAR(vector_res, uint, W2, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHRUN_N1(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQSHRUN_N2(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_VQSHRUN_N(T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQSHRUN_N1(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT)


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

  /* Choose shift amount arbitrarily.  */
#define CMT " (negative input)"
  TEST_VQSHRUN_N(int, s, 16, 8, 8, 3, expected_cumulative_sat_neg, CMT);
  TEST_VQSHRUN_N(int, s, 32, 16, 4, 4, expected_cumulative_sat_neg, CMT);
  TEST_VQSHRUN_N(int, s, 64, 32, 2, 2, expected_cumulative_sat_neg, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg, CMT);

  
  /* Fill input vector with max value, to check saturation on
     limits.  */
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);

#undef CMT
#define CMT " (check cumulative saturation)"
  TEST_VQSHRUN_N(int, s, 16, 8, 8, 1, expected_cumulative_sat_max_sh1, CMT);
  TEST_VQSHRUN_N(int, s, 32, 16, 4, 1, expected_cumulative_sat_max_sh1, CMT);
  TEST_VQSHRUN_N(int, s, 64, 32, 2, 1, expected_cumulative_sat_max_sh1, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh1, CMT);

  
  /* Fill input vector with positive values, to check normal case.  */
  VDUP(vector, q, int, s, 16, 8, 0x1234);
  VDUP(vector, q, int, s, 32, 4, 0x87654321);
  VDUP(vector, q, int, s, 64, 2, 0xDEADBEEF);

#undef CMT
#define CMT ""
  TEST_VQSHRUN_N(int, s, 16, 8, 8, 6, expected_cumulative_sat, CMT);
  TEST_VQSHRUN_N(int, s, 32, 16, 4, 7, expected_cumulative_sat, CMT);
  TEST_VQSHRUN_N(int, s, 64, 32, 2, 8, expected_cumulative_sat, CMT);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
}

int main (void)
{
  exec_vqshrun_n ();
  return 0;
}
