#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results with positive input.  */
VECT_VAR_DECL(expected_positive,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_positive,uint,32,4) [] = { 0xbf000000, 0xbf000000,
						  0xbf000000, 0xbf000000 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_positive, hfloat, 16, 4) [] = { 0x3834, 0x3834,
						       0x3834, 0x3834 };
VECT_VAR_DECL(expected_positive, hfloat, 16, 8) [] = { 0x2018, 0x2018,
						       0x2018, 0x2018,
						       0x2018, 0x2018,
						       0x2018, 0x2018 };
#endif
VECT_VAR_DECL(expected_positive,hfloat,32,2) [] = { 0x3f068000, 0x3f068000 };
VECT_VAR_DECL(expected_positive,hfloat,32,4) [] = { 0x3c030000, 0x3c030000,
						    0x3c030000, 0x3c030000 };

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_negative,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_negative,uint,32,4) [] = { 0xee800000, 0xee800000,
						  0xee800000, 0xee800000 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_negative, hfloat, 16, 4) [] = { 0xae64, 0xae64,
						       0xae64, 0xae64 };
VECT_VAR_DECL(expected_negative, hfloat, 16, 8) [] = { 0xa018, 0xa018,
						       0xa018, 0xa018,
						       0xa018, 0xa018,
						       0xa018, 0xa018 };
#endif
VECT_VAR_DECL(expected_negative,hfloat,32,2) [] = { 0xbdcc8000, 0xbdcc8000 };
VECT_VAR_DECL(expected_negative,hfloat,32,4) [] = { 0xbc030000, 0xbc030000,
						    0xbc030000, 0xbc030000 };

/* Expected results with FP special values (NaN, infinity).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp1, hfloat, 16, 4) [] = { 0x7e00, 0x7e00,
						  0x7e00, 0x7e00 };
VECT_VAR_DECL(expected_fp1, hfloat, 16, 8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
#endif
VECT_VAR_DECL(expected_fp1,hfloat,32,2) [] = { 0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_fp1,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results with FP special values (zero, large value).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp2, hfloat, 16, 4) [] = { 0x7c00, 0x7c00,
						  0x7c00, 0x7c00 };
VECT_VAR_DECL(expected_fp2, hfloat, 16, 8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
#endif
VECT_VAR_DECL(expected_fp2,hfloat,32,2) [] = { 0x7f800000, 0x7f800000 };
VECT_VAR_DECL(expected_fp2,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results with FP special values (-0, -infinity).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp3, hfloat, 16, 4) [] = { 0xfc00, 0xfc00,
						  0xfc00, 0xfc00};
VECT_VAR_DECL(expected_fp3, hfloat, 16, 8) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000 };
#endif
VECT_VAR_DECL(expected_fp3,hfloat,32,2) [] = { 0xff800000, 0xff800000 };
VECT_VAR_DECL(expected_fp3,hfloat,32,4) [] = { 0x80000000, 0x80000000,
					       0x80000000, 0x80000000 };

/* Expected results with FP special large negative value.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp4, hfloat, 16, 4) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000 };
#endif
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
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);
#endif
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, float, 32, 4);

  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif
  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 4);

  clean_results ();

  /* Choose init value arbitrarily, positive.  */
  VDUP(vector, , uint, u, 32, 2, 0x12345678);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, 1.9f);
#endif
  VDUP(vector, , float, f, 32, 2, 1.9f);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, q, float, f, 16, 8, 125.0f);
#endif
  VDUP(vector, q, uint, u, 32, 4, 0xABCDEF10);
  VDUP(vector, q, float, f, 32, 4, 125.0f);

  /* Apply the operator.  */
  TEST_VRECPE(, uint, u, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(, float, f, 16, 4);
#endif
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, uint, u, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(q, float, f, 16, 8);
#endif
  TEST_VRECPE(q, float, f, 32, 4);

#define CMT " (positive input)"
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_positive, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_positive, CMT);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_positive, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_positive, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_positive, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_positive, CMT);

  /* Choose init value arbitrarily,negative.  */
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, -10.0f);
#endif
  VDUP(vector, , float, f, 32, 2, -10.0f);
  VDUP(vector, q, uint, u, 32, 4, 0x89081234);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, q, float, f, 16, 8, -125.0f);
#endif
  VDUP(vector, q, float, f, 32, 4, -125.0f);

  /* Apply the operator.  */
  TEST_VRECPE(, uint, u, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(, float, f, 16, 4);
#endif
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, uint, u, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(q, float, f, 16, 8);
#endif
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " (negative input)"
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_negative, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_negative, CMT);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_negative, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_negative, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_negative, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_negative, CMT);

  /* Test FP variants with special input values (NaN, infinity).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, NAN);
  VDUP(vector, q, float, f, 16, 8, HUGE_VALF);
#endif
  VDUP(vector, , float, f, 32, 2, NAN);
  VDUP(vector, q, float, f, 32, 4, HUGE_VALF);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(, float, f, 16, 4);
  TEST_VRECPE(q, float, f, 16, 8);
#endif
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (NaN, infinity)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp1, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp1, CMT);

  /* Test FP variants with special input values (zero, large value).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, 0.0f);
  VDUP(vector, q, float, f, 16, 8, 8.97229e37f /*9.0e37f*/);
#endif
  VDUP(vector, , float, f, 32, 2, 0.0f);
  VDUP(vector, q, float, f, 32, 4, 8.97229e37f /*9.0e37f*/);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(, float, f, 16, 4);
  TEST_VRECPE(q, float, f, 16, 8);
#endif
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (zero, large value)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp2, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp2, CMT);

  /* Test FP variants with special input values (-0, -infinity).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, -0.0f);
  VDUP(vector, q, float, f, 16, 8, -HUGE_VALF);
#endif
  VDUP(vector, , float, f, 32, 2, -0.0f);
  VDUP(vector, q, float, f, 32, 4, -HUGE_VALF);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(, float, f, 16, 4);
  TEST_VRECPE(q, float, f, 16, 8);
#endif
  TEST_VRECPE(, float, f, 32, 2);
  TEST_VRECPE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (-0, -infinity)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp3, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp3, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp3, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp3, CMT);

  /* Test FP variants with special input values (large negative value).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, -9.0e37f);
#endif
  VDUP(vector, , float, f, 32, 2, -9.0e37f);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPE(, float, f, 16, 4);
#endif
  TEST_VRECPE(, float, f, 32, 2);

#undef CMT
#define CMT " FP special (large negative value)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp4, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp4, CMT);
}

int main (void)
{
  exec_vrecpe ();
  return 0;
}
