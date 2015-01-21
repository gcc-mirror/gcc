#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0x19, 0x19, 0x19, 0x19 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x4, 0x4 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x10, 0x10, 0x10, 0x10,
					0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xa, 0xa, 0xa, 0xa };

/* Expected values of cumulative_saturation flag when saturation
   occurs.  */
int VECT_VAR(expected_cumulative_sat2,int,16,4) = 1;
int VECT_VAR(expected_cumulative_sat2,int,32,2) = 1;
int VECT_VAR(expected_cumulative_sat2,int,16,8) = 1;
int VECT_VAR(expected_cumulative_sat2,int,32,4) = 1;

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected2,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected2,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected2,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
					 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected2,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					 0x7fffffff, 0x7fffffff };

#define INSN_NAME vqdmulh
#define TEST_MSG "VQDMULH_N"
#define FNNAME1(NAME) exec_ ## NAME ## _n
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  int i;

  /* vector_res = vqdmulh_n(vector,val), then store the result.  */
#define TEST_VQDMULH_N2(INSN, Q, T1, T2, W, N, L, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));	\
  VECT_VAR(vector_res, T1, W, N) =				\
    INSN##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),		\
			L);					\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),			\
		    VECT_VAR(vector_res, T1, W, N));		\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VQDMULH_N1(INSN, Q, T1, T2, W, N, L, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQDMULH_N2(INSN, Q, T1, T2, W, N, L, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_VQDMULH_N(Q, T1, T2, W, N, L, EXPECTED_CUMULATIVE_SAT, CMT)	\
  TEST_VQDMULH_N1(INSN_NAME, Q, T1, T2, W, N, L, EXPECTED_CUMULATIVE_SAT, CMT)

  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);

  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);

  clean_results ();

  /* Initialize vector.  */
  VDUP(vector, , int, s, 16, 4, 0x1000);
  VDUP(vector, , int, s, 32, 2, 0x100023);
  VDUP(vector, q, int, s, 16, 8, 0x1000);
  VDUP(vector, q, int, s, 32, 4, 0x100045);

  /* Choose multiplier arbitrarily.  */
  TEST_VQDMULH_N(, int, s, 16, 4, 0xCF, expected_cumulative_sat, "");
  TEST_VQDMULH_N(, int, s, 32, 2, 0x2344, expected_cumulative_sat, "");
  TEST_VQDMULH_N(q, int, s, 16, 8, 0x80, expected_cumulative_sat, "");
  TEST_VQDMULH_N(q, int, s, 32, 4, 0x5422, expected_cumulative_sat, "");

  CHECK (TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK (TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK (TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected, "");

  /* Choose input values to trigger saturation.  */
  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);

#define TEST_MSG2 " (check mul cumulative saturation)"
  TEST_VQDMULH_N(, int, s, 16, 4, 0x8000, expected_cumulative_sat2, TEST_MSG2);
  TEST_VQDMULH_N(, int, s, 32, 2, 0x80000000, expected_cumulative_sat2, TEST_MSG2);
  TEST_VQDMULH_N(q, int, s, 16, 8, 0x8000, expected_cumulative_sat2, TEST_MSG2);
  TEST_VQDMULH_N(q, int, s, 32, 4, 0x80000000, expected_cumulative_sat2, TEST_MSG2);

  CHECK (TEST_MSG, int, 16, 4, PRIx16, expected2, TEST_MSG2);
  CHECK (TEST_MSG, int, 32, 2, PRIx32, expected2, TEST_MSG2);
  CHECK (TEST_MSG, int, 16, 8, PRIx16, expected2, TEST_MSG2);
  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected2, TEST_MSG2);
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
