#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0x200, 0x1c2, 0x188, 0x152 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x200, 0x1c2 };

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected2,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0x7fffffffffffffff,
					 0x7fffffffffffffff };

#define INSN_NAME vqdmull
#define TEST_MSG "VQDMULL"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=vqdmull(x,x), then store the result.  */
#define TEST_VQDMULL2(INSN, T1, T2, W, W2, N, CMT)	\
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W2, N));	\
  VECT_VAR(vector_res, T1, W2, N) =				\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		   VECT_VAR(vector2, T1, W, N));		\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N),			\
		 VECT_VAR(vector_res, T1, W2, N))

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VQDMULL1(INSN, T1, T2, W, W2, N, CMT)	\
  TEST_VQDMULL2(INSN, T1, T2, W, W2, N, CMT)

#define TEST_VQDMULL(T1, T2, W, W2, N, CMT)	\
  TEST_VQDMULL1(INSN_NAME, T1, T2, W, W2, N, CMT)

  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);

  clean_results ();

  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector2, buffer, , int, s, 16, 4);
  VLOAD(vector2, buffer, , int, s, 32, 2);

  TEST_VQDMULL(int, s, 16, 32, 4, "");
  TEST_VQDMULL(int, s, 32, 64, 2, "");

  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK (TEST_MSG, int, 64, 2, PRIx64, expected, "");

  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector2, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector2, , int, s, 32, 2, 0x80000000);

#define TEST_MSG2 "with saturation"
  TEST_VQDMULL(int, s, 16, 32, 4, TEST_MSG2);
  TEST_VQDMULL(int, s, 32, 64, 2, TEST_MSG2);

  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected2, TEST_MSG2);
  CHECK (TEST_MSG, int, 64, 2, PRIx64, expected2, TEST_MSG2);
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
