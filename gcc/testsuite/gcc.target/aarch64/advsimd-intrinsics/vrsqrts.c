#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xd3cb, 0xd3cb, 0xd3cb, 0xd3cb };
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xc726, 0xc726, 0xc726, 0xc726,
					      0xc726, 0xc726, 0xc726, 0xc726 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc2796b84, 0xc2796b84 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc0e4a3d8, 0xc0e4a3d8,
					   0xc0e4a3d8, 0xc0e4a3d8 };

/* Expected results with input=NaN.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_nan, hfloat, 16, 4) [] = { 0x7e00, 0x7e00,
						  0x7e00, 0x7e00 };
VECT_VAR_DECL(expected_nan, hfloat, 16, 8) [] = { 0x7e00, 0x7e00,
						  0x7e00, 0x7e00,
						  0x7e00, 0x7e00,
						  0x7e00, 0x7e00 };
#endif
VECT_VAR_DECL(expected_nan,hfloat,32,2) [] = { 0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_nan,hfloat,32,4) [] = { 0x7fc00000, 0x7fc00000,
					       0x7fc00000, 0x7fc00000 };

/* Expected results with FP special inputs values (infinity, 0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp1, hfloat, 16, 4) [] = { 0xfc00, 0xfc00,
						  0xfc00, 0xfc00 };
VECT_VAR_DECL(expected_fp1, hfloat, 16, 8) [] = { 0x3e00, 0x3e00,
						  0x3e00, 0x3e00,
						  0x3e00, 0x3e00,
						  0x3e00, 0x3e00 };
#endif
VECT_VAR_DECL(expected_fp1,hfloat,32,2) [] = { 0xff800000, 0xff800000 };
VECT_VAR_DECL(expected_fp1,hfloat,32,4) [] = { 0x3fc00000, 0x3fc00000,
					       0x3fc00000, 0x3fc00000 };

/* Expected results with only FP special inputs values (infinity,
   0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_fp2, hfloat, 16, 4) [] = { 0x3e00, 0x3e00,
						  0x3e00, 0x3e00 };
VECT_VAR_DECL(expected_fp2, hfloat, 16, 8) [] = { 0x3e00, 0x3e00,
						  0x3e00, 0x3e00,
						  0x3e00, 0x3e00,
						  0x3e00, 0x3e00 };
#endif
VECT_VAR_DECL(expected_fp2,hfloat,32,2) [] = { 0x3fc00000, 0x3fc00000 };
VECT_VAR_DECL(expected_fp2,hfloat,32,4) [] = { 0x3fc00000, 0x3fc00000,
					       0x3fc00000, 0x3fc00000 };

#define TEST_MSG "VRSQRTS/VRSQRTSQ"
void exec_vrsqrts(void)
{
  int i;

  /* Basic test: y=vrsqrts(x), then store the result.  */
#define TEST_VRSQRTS(Q, T1, T2, W, N)			\
  VECT_VAR(vector_res, T1, W, N) =			\
    vrsqrts##Q##_##T2##W(VECT_VAR(vector, T1, W, N),	\
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
  VDUP(vector, q, float, f, 16, 8, 9.1f);
#endif
  VDUP(vector, , float, f, 32, 2, 12.9f);
  VDUP(vector, q, float, f, 32, 4, 9.1f);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector2, , float, f, 16, 4, 9.9f);
  VDUP(vector2, q, float, f, 16, 8, 1.9f);
#endif
  VDUP(vector2, , float, f, 32, 2, 9.9f);
  VDUP(vector2, q, float, f, 32, 4, 1.9f);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRSQRTS(, float, f, 16, 4);
  TEST_VRSQRTS(q, float, f, 16, 8);
#endif
  TEST_VRSQRTS(, float, f, 32, 2);
  TEST_VRSQRTS(q, float, f, 32, 4);

#define CMT ""
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
  TEST_VRSQRTS(, float, f, 16, 4);
  TEST_VRSQRTS(q, float, f, 16, 8);
#endif
  TEST_VRSQRTS(, float, f, 32, 2);
  TEST_VRSQRTS(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (NAN) and normal values"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_nan, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_nan, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_nan, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_nan, CMT);


  /* Test FP variants with special input values (infinity, 0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, HUGE_VALF);
  VDUP(vector, q, float, f, 16, 8, 0.0f);
  /* Restore a normal value in vector2.  */
  VDUP(vector2, q, float, f, 16, 8, 3.2f);
#endif
  VDUP(vector, , float, f, 32, 2, HUGE_VALF);
  VDUP(vector, q, float, f, 32, 4, 0.0f);
  /* Restore a normal value in vector2.  */
  VDUP(vector2, q, float, f, 32, 4, 3.2f);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRSQRTS(, float, f, 16, 4);
  TEST_VRSQRTS(q, float, f, 16, 8);
#endif
  TEST_VRSQRTS(, float, f, 32, 2);
  TEST_VRSQRTS(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (infinity, 0) and normal values"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp1, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp1, CMT);


  /* Test FP variants with only special input values (infinity, 0).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, HUGE_VALF);
  VDUP(vector, q, float, f, 16, 8, 0.0f);
  VDUP(vector2, , float, f, 16, 4, -0.0f);
  VDUP(vector2, q, float, f, 16, 8, HUGE_VALF);
#endif
  VDUP(vector, , float, f, 32, 2, HUGE_VALF);
  VDUP(vector, q, float, f, 32, 4, 0.0f);
  VDUP(vector2, , float, f, 32, 2, -0.0f);
  VDUP(vector2, q, float, f, 32, 4, HUGE_VALF);

  /* Apply the operator.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRSQRTS(, float, f, 16, 4);
  TEST_VRSQRTS(q, float, f, 16, 8);
#endif
  TEST_VRSQRTS(, float, f, 32, 2);
  TEST_VRSQRTS(q, float, f, 32, 4);

#undef CMT
#define CMT " only FP special (infinity, 0)"
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_fp2, CMT);
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp2, CMT);
}

int main (void)
{
  exec_vrsqrts ();
  return 0;
}
