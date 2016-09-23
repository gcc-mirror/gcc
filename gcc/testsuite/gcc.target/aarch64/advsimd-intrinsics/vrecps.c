#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results with positive input.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xd70c, 0xd70c, 0xd70c, 0xd70c };
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xcedc, 0xcedc, 0xcedc, 0xcedc,
					      0xcedc, 0xcedc, 0xcedc, 0xcedc };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc2e19eb7, 0xc2e19eb7 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1db851f, 0xc1db851f,
					   0xc1db851f, 0xc1db851f };

/* Expected results with FP special values (NaN).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp1, hfloat, 16, 4) [] = { 0x7e00, 0x7e00,
						  0x7e00, 0x7e00 };
VECT_VAR_DECL(expected_fp1, hfloat, 16, 8) [] = { 0x7e00, 0x7e00,
						  0x7e00, 0x7e00,
						  0x7e00, 0x7e00,
						  0x7e00, 0x7e00 };
#endif
VECT_VAR_DECL(expected_fp1,hfloat,32,2) [] = { 0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_fp1,hfloat,32,4) [] = { 0x7fc00000, 0x7fc00000,
					       0x7fc00000, 0x7fc00000 };

/* Expected results with FP special values (infinity, 0) and normal
   values.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp2, hfloat, 16, 4) [] = { 0xfc00, 0xfc00,
						  0xfc00, 0xfc00 };
VECT_VAR_DECL(expected_fp2, hfloat, 16, 8) [] = { 0x4000, 0x4000,
						  0x4000, 0x4000,
						  0x4000, 0x4000,
						  0x4000, 0x4000 };
#endif
VECT_VAR_DECL(expected_fp2,hfloat,32,2) [] = { 0xff800000, 0xff800000 };
VECT_VAR_DECL(expected_fp2,hfloat,32,4) [] = { 0x40000000, 0x40000000,
					       0x40000000, 0x40000000 };

/* Expected results with FP special values (infinity, 0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp3, hfloat, 16, 4) [] = { 0x4000, 0x4000,
						  0x4000, 0x4000 };
VECT_VAR_DECL(expected_fp3, hfloat, 16, 8) [] = { 0x4000, 0x4000,
						  0x4000, 0x4000,
						  0x4000, 0x4000,
						  0x4000, 0x4000 };
#endif
VECT_VAR_DECL(expected_fp3,hfloat,32,2) [] = { 0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_fp3,hfloat,32,4) [] = { 0x40000000, 0x40000000,
					       0x40000000, 0x40000000 };

#define TEST_MSG "VRECPS/VRECPSQ"
void exec_vrecps(void)
{
  int i;

  /* Basic test: y=vrecps(x), then store the result.  */
#define TEST_VRECPS(Q, T1, T2, W, N)			\
  VECT_VAR(vector_res, T1, W, N) =			\
    vrecps##Q##_##T2##W(VECT_VAR(vector, T1, W, N),	\
			VECT_VAR(vector2, T1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),		\
		    VECT_VAR(vector_res, T1, W, N))

  /* No need for integer variants.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);
#endif
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, float, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector2, float, 16, 4);
  DECL_VARIABLE(vector2, float, 16, 8);
#endif
  DECL_VARIABLE(vector2, float, 32, 2);
  DECL_VARIABLE(vector2, float, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif
  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 4);

  clean_results ();

  /* Choose init value arbitrarily.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, 12.9f);
  VDUP(vector, q, float, f, 16, 8, 9.2f);
#endif
  VDUP(vector, , float, f, 32, 2, 12.9f);
  VDUP(vector, q, float, f, 32, 4, 9.2f);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector2, , float, f, 16, 4, 8.9f);
  VDUP(vector2, q, float, f, 16, 8, 3.2f);
#endif
  VDUP(vector2, , float, f, 32, 2, 8.9f);
  VDUP(vector2, q, float, f, 32, 4, 3.2f);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPS(, float, f, 16, 4);
  TEST_VRECPS(q, float, f, 16, 8);
#endif
  TEST_VRECPS(, float, f, 32, 2);
  TEST_VRECPS(q, float, f, 32, 4);

#define CMT " (positive input)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, CMT);


  /* Test FP variants with special input values (NaN).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, NAN);
  VDUP(vector2, q, float, f, 16, 8, NAN);
#endif
  VDUP(vector, , float, f, 32, 2, NAN);
  VDUP(vector2, q, float, f, 32, 4, NAN);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPS(, float, f, 16, 4);
  TEST_VRECPS(q, float, f, 16, 8);
#endif
  TEST_VRECPS(, float, f, 32, 2);
  TEST_VRECPS(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (NaN)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp1, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp1, CMT);


  /* Test FP variants with special input values (infinity, 0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, HUGE_VALF);
  VDUP(vector, q, float, f, 16, 8, 0.0f);
  VDUP(vector2, q, float, f, 16, 8, 3.2f); /* Restore a normal value.  */
#endif
  VDUP(vector, , float, f, 32, 2, HUGE_VALF);
  VDUP(vector, q, float, f, 32, 4, 0.0f);
  VDUP(vector2, q, float, f, 32, 4, 3.2f); /* Restore a normal value.  */


  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPS(, float, f, 16, 4);
  TEST_VRECPS(q, float, f, 16, 8);
#endif
  TEST_VRECPS(, float, f, 32, 2);
  TEST_VRECPS(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (infinity, 0) and normal value"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp2, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp2, CMT);


  /* Test FP variants with only special input values (infinity, 0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, HUGE_VALF);
  VDUP(vector, q, float, f, 16, 8, 0.0f);
  VDUP(vector2, , float, f, 16, 4, 0.0f);
  VDUP(vector2, q, float, f, 16, 8, HUGE_VALF);
#endif
  VDUP(vector, , float, f, 32, 2, HUGE_VALF);
  VDUP(vector, q, float, f, 32, 4, 0.0f);
  VDUP(vector2, , float, f, 32, 2, 0.0f);
  VDUP(vector2, q, float, f, 32, 4, HUGE_VALF);


  /* Apply the operator */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRECPS(, float, f, 16, 4);
  TEST_VRECPS(q, float, f, 16, 8);
#endif
  TEST_VRECPS(, float, f, 32, 2);
  TEST_VRECPS(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (infinity, 0)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp3, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp3, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp3, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp3, CMT);
}

int main (void)
{
  exec_vrecps ();
  return 0;
}
