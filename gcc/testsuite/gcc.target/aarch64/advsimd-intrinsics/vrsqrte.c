#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results.  */
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x9c800000, 0x9c800000,
					 0x9c800000, 0x9c800000 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x3e498000, 0x3e498000 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x3e700000, 0x3e700000,
					   0x3e700000, 0x3e700000 };

/* Expected results with large uint #1.  */
VECT_VAR_DECL(expected_1,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_1,uint,32,4) [] = { 0xae800000, 0xae800000,
					   0xae800000, 0xae800000 };

/* Expected results with large uint #2.  */
VECT_VAR_DECL(expected_2,uint,32,2) [] = { 0xb4800000, 0xb4800000 };
VECT_VAR_DECL(expected_2,uint,32,4) [] = { 0xed000000, 0xed000000,
					   0xed000000, 0xed000000 };

/* Expected results with FP special inputs values (NaNs, ...).  */
VECT_VAR_DECL(expected_fp1,hfloat,32,2) [] = { 0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_fp1,hfloat,32,4) [] = { 0x7f800000, 0x7f800000,
					       0x7f800000, 0x7f800000 };

/* Expected results with FP special inputs values
   (negative, infinity).  */
VECT_VAR_DECL(expected_fp2,hfloat,32,2) [] = { 0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_fp2,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results with FP special inputs values
   (-0, -infinity).  */
VECT_VAR_DECL(expected_fp3,hfloat,32,2) [] = { 0xff800000, 0xff800000 };
VECT_VAR_DECL(expected_fp3,hfloat,32,4) [] = { 0x7fc00000, 0x7fc00000,
					       0x7fc00000, 0x7fc00000 };

#define TEST_MSG "VRSQRTE/VRSQRTEQ"
void exec_vrsqrte(void)
{
  int i;

  /* Basic test: y=vrsqrte(x), then store the result.  */
#define TEST_VRSQRTE(Q, T1, T2, W, N)			\
  VECT_VAR(vector_res, T1, W, N) =			\
    vrsqrte##Q##_##T2##W(VECT_VAR(vector, T1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),		\
		    VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, float, 32, 4);

  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, float, 32, 4);

  clean_results ();

  /* Choose init value arbitrarily.  */
  VDUP(vector, , uint, u, 32, 2, 0x12345678);
  VDUP(vector, , float, f, 32, 2, 25.799999f);
  VDUP(vector, q, uint, u, 32, 4, 0xABCDEF10);
  VDUP(vector, q, float, f, 32, 4, 18.2f);

  /* Apply the operator.  */
  TEST_VRSQRTE(, uint, u, 32, 2);
  TEST_VRSQRTE(, float, f, 32, 2);
  TEST_VRSQRTE(q, uint, u, 32, 4);
  TEST_VRSQRTE(q, float, f, 32, 4);

#define CMT ""
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, CMT);


  /* Don't test FP variants with negative inputs.  */
  /* Use input with various values of bits 30 and 31.  */
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector, q, uint, u, 32, 4, 0x89081234);

  /* Apply the operator.  */
  TEST_VRSQRTE(, uint, u, 32, 2);
  TEST_VRSQRTE(q, uint, u, 32, 4);

#undef CMT
#define CMT " (large uint #1)"
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_1, CMT);


  /* Choose init value arbitrarily.  */
  VDUP(vector, , uint, u, 32, 2, 0x80000000);
  VDUP(vector, q, uint, u, 32, 4, 0x4ABCDEF0);

  /* Apply the operator.  */
  TEST_VRSQRTE(, uint, u, 32, 2);
  TEST_VRSQRTE(q, uint, u, 32, 4);

#undef CMT
#define CMT " (large uint #2)"
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_2, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_2, CMT);


  /* Test FP variants with special input values (NaNs, ...).  */
  VDUP(vector, , float, f, 32, 2, NAN);
  VDUP(vector, q, float, f, 32, 4, 0.0f);

  /* Apply the operator.  */
  TEST_VRSQRTE(, float, f, 32, 2);
  TEST_VRSQRTE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (NaN, 0)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp1, CMT);


  /* Test FP variants with special input values (negative, infinity).  */
  VDUP(vector, , float, f, 32, 2, -1.0f);
  VDUP(vector, q, float, f, 32, 4, HUGE_VALF);

  /* Apply the operator.  */
  TEST_VRSQRTE(, float, f, 32, 2);
  TEST_VRSQRTE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (negative, infinity)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp2, CMT);

  /* Test FP variants with special input values (-0, -infinity).  */
  VDUP(vector, , float, f, 32, 2, -0.0f);
  VDUP(vector, q, float, f, 32, 4, -HUGE_VALF);

  /* Apply the operator.  */
  TEST_VRSQRTE(, float, f, 32, 2);
  TEST_VRSQRTE(q, float, f, 32, 4);

#undef CMT
#define CMT " FP special (-0, -infinity)"
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_fp3, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_fp3, CMT);
}

int main (void)
{
  exec_vrsqrte ();
  return 0;
}
