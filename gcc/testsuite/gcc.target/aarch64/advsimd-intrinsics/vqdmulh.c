#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffff, 0xffffffff,
					0xffffffff, 0xffffffff };

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
#define TEST_MSG "VQDMULH"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = vqdmulh(vector,vector2,lane), then store the result.  */
#define TEST_VQDMULH2(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VQDMULH1(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQDMULH2(INSN, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_VQDMULH(Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)	\
  TEST_VQDMULH1(INSN_NAME, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

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
  VDUP(vector2, , int, s, 16, 4, 0x55);
  VDUP(vector2, , int, s, 32, 2, 0xBB);
  VDUP(vector2, q, int, s, 16, 8, 0x33);
  VDUP(vector2, q, int, s, 32, 4, 0x22);

  TEST_VQDMULH(, int, s, 16, 4, expected_cumulative_sat, "");
  TEST_VQDMULH(, int, s, 32, 2, expected_cumulative_sat, "");
  TEST_VQDMULH(q, int, s, 16, 8, expected_cumulative_sat, "");
  TEST_VQDMULH(q, int, s, 32, 4, expected_cumulative_sat, "");

  CHECK (TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK (TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK (TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected, "");

  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector2, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector2, , int, s, 32, 2, 0x80000000);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector2, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector2, q, int, s, 32, 4, 0x80000000);

#define TEST_MSG2 "with saturation"
  TEST_VQDMULH(, int, s, 16, 4, expected_cumulative_sat2, TEST_MSG2);
  TEST_VQDMULH(, int, s, 32, 2, expected_cumulative_sat2, TEST_MSG2);
  TEST_VQDMULH(q, int, s, 16, 8, expected_cumulative_sat2, TEST_MSG2);
  TEST_VQDMULH(q, int, s, 32, 4, expected_cumulative_sat2, TEST_MSG2);

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
