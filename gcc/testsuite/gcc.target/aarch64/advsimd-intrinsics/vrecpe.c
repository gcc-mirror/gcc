#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results with positive input.  */
VECT_VAR_DECL(expected_positive,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_positive,uint,32,4) [] = { 0xbf000000, 0xbf000000,
						  0xbf000000, 0xbf000000 };
VECT_VAR_DECL(expected_positive,hfloat,32,2) [] = { 0x3f068000, 0x3f068000 };
VECT_VAR_DECL(expected_positive,hfloat,32,4) [] = { 0x3c030000, 0x3c030000,
						    0x3c030000, 0x3c030000 };

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_negative,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_negative,uint,32,4) [] = { 0xee800000, 0xee800000,
						  0xee800000, 0xee800000 };
VECT_VAR_DECL(expected_negative,hfloat,32,2) [] = { 0xbdcc8000, 0xbdcc8000 };
VECT_VAR_DECL(expected_negative,hfloat,32,4) [] = { 0xbc030000, 0xbc030000,
						    0xbc030000, 0xbc030000 };

/* Expected results with FP special values (NaN, infinity).  */
VECT_VAR_DECL(expected_fp1,hfloat,32,2) [] = { 0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_fp1,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results with FP special values (zero, large value).  */
VECT_VAR_DECL(expected_fp2,hfloat,32,2) [] = { 0x7f800000, 0x7f800000 };
VECT_VAR_DECL(expected_fp2,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results with FP special values (-0, -infinity).  */
VECT_VAR_DECL(expected_fp3,hfloat,32,2) [] = { 0xff800000, 0xff800000 };
VECT_VAR_DECL(expected_fp3,hfloat,32,4) [] = { 0x80000000, 0x80000000,
					       0x80000000, 0x80000000 };

/* Expected results with FP special large negative value.  */
VECT_VAR_DECL(expected_fp4,hfloat,32,2) [] = { 0x80000000, 0x80000000 };

#define TEST_MSG "VRECPE/VRECPEQ"
void exec_vrecpe(void)
{
  int i;

  /* Basic test: y=vrecpe(x), then store the result.  */
#define TEST_VRECPE(Q, T1, T2, W, N)			\
  VECT_VAR(vector_res, T1, W, N) =			\
    vrecpe##Q##_##T2##W(VECT_VAR(vector, T1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),		\
		    VECT_VAR(vector_res, T1, W, N))

  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, float, 32, 4);

  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 4);

  clean_results ();

  /* Choose init value arbitrarily, positive.  */
  VDUP(vector, , uint, u, 32, 2, 0x12345678);
  VDUP(vector, , float, f, 32, 2, 1.9f);
  VDUP(vector, q, uint, u, 32, 4, 0xABCDEF10);
  VDUP(vector, q, float, f, 32, 4, 125.0f);

  /* Apply the operator.  */
  TEST_VRECPE(, uint, u, 32, 2);
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, uint, u, 32, 4);
  TEST_VRECPE(q, float, f, 32, 4);

#define CMT " (positive input)"
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_positive, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_positive, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_positive, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_positive, CMT);

  /* Choose init value arbitrarily,negative.  */
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector, , float, f, 32, 2, -10.0f);
  VDUP(vector, q, uint, u, 32, 4, 0x89081234);
  VDUP(vector, q, float, f, 32, 4, -125.0f);

  /* Apply the operator.  */
  TEST_VRECPE(, uint, u, 32, 2);
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, uint, u, 32, 4);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " (negative input)"
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_negative, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_negative, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_negative, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_negative, CMT);

  /* Test FP variants with special input values (NaN, infinity).  */
  VDUP(vector, , float, f, 32, 2, NAN);
  VDUP(vector, q, float, f, 32, 4, HUGE_VALF);

  /* Apply the operator.  */
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (NaN, infinity)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp1, CMT);

  /* Test FP variants with special input values (zero, large value).  */
  VDUP(vector, , float, f, 32, 2, 0.0f);
  VDUP(vector, q, float, f, 32, 4, 8.97229e37f /*9.0e37f*/);

  /* Apply the operator.  */
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (zero, large value)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp2, CMT);

  /* Test FP variants with special input values (-0, -infinity).  */
  VDUP(vector, , float, f, 32, 2, -0.0f);
  VDUP(vector, q, float, f, 32, 4, -HUGE_VALF);

  /* Apply the operator.  */
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (-0, -infinity)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp3, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp3, CMT);

  /* Test FP variants with special input values (large negative value).  */
  VDUP(vector, , float, f, 32, 2, -9.0e37f);

  /* Apply the operator.  */
  TEST_VRECPE(, float, f, 32, 2);

#undef CMT
#define CMT " FP special (large negative value)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp4, CMT);
}

int main (void)
{
  exec_vrecpe ();
  return 0;
}
