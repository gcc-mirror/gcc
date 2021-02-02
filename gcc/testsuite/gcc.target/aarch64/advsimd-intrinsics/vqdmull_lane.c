#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0x8000, 0x8000, 0x8000, 0x8000 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x4000, 0x4000 };

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected2,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0x7fffffffffffffff,
					 0x7fffffffffffffff };

#define INSN_NAME vqdmull
#define TEST_MSG "VQDMULL_LANE"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  int i;

  /* vector_res = vqdmull_lane(vector,vector2,lane), then store the result.  */
#define TEST_VQDMULL_LANE2(INSN, T1, T2, W, W2, N, L, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W2, N));		\
  VECT_VAR(vector_res, T1, W2, N) =					\
    INSN##_lane_##T2##W(VECT_VAR(vector, T1, W, N),			\
			VECT_VAR(vector2, T1, W, N),			\
			L);						\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N),				\
		 VECT_VAR(vector_res, T1, W2, N))

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VQDMULL_LANE1(INSN, T1, T2, W, W2, N, L, CMT) \
  TEST_VQDMULL_LANE2(INSN, T1, T2, W, W2, N, L, CMT)

#define TEST_VQDMULL_LANE(T1, T2, W, W2, N, L, CMT) \
  TEST_VQDMULL_LANE1(INSN_NAME, T1, T2, W, W2, N, L, CMT)

  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);

  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);

  clean_results ();

  /* Initialize vector.  */
  VDUP(vector, , int, s, 16, 4, 0x1000);
  VDUP(vector, , int, s, 32, 2, 0x1000);

  /* Initialize vector2.  */
  VDUP(vector2, , int, s, 16, 4, 0x4);
  VDUP(vector2, , int, s, 32, 2, 0x2);

  /* Choose lane arbitrarily.  */
  TEST_VQDMULL_LANE(int, s, 16, 32, 4, 2, "");
  TEST_VQDMULL_LANE(int, s, 32, 64, 2, 1, "");

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");

  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector2, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector2, , int, s, 32, 2, 0x80000000);

#define TEST_MSG2 "with saturation"
  TEST_VQDMULL_LANE(int, s, 16, 32, 4, 2, TEST_MSG2);
  TEST_VQDMULL_LANE(int, s, 32, 64, 2, 1, TEST_MSG2);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected2, TEST_MSG2);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected2, TEST_MSG2);
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
